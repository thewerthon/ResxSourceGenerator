namespace ResxSourceGenerator;

using System.Diagnostics.CodeAnalysis;
using System.Globalization;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;
using System.Xml.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Text;

internal static class BuildHelper
{
	private const int MaxDocCommentLength = 256;
	private const string HelpLinkUri = "https://github.com/thewerthon/ResxSourceGenerator/tree/main/README.md#Diagnostics";

	private static readonly DiagnosticDescriptor EmptyWarning = new(
		id: "ResX001",
		title: "Empty resource file",
		messageFormat: "Resource file generated without any members",
		category: "Globalization",
		defaultSeverity: DiagnosticSeverity.Warning,
		helpLinkUri: HelpLinkUri,
		isEnabledByDefault: true
	);

	private static readonly DiagnosticDescriptor InvalidKeyWarning = new(
		id: "ResX002",
		title: "Invalid Key in resource file",
		messageFormat: "Entry with key '{0}' is invalid and will be ignored",
		category: "Globalization",
		defaultSeverity: DiagnosticSeverity.Warning,
		helpLinkUri: HelpLinkUri,
		isEnabledByDefault: true
	);

	private static readonly DiagnosticDescriptor MissingValueWarning = new(
		id: "ResX003",
		title: "Missing Value in resource file",
		messageFormat: "Entry with key '{0}' is has no value and will be ignored",
		category: "Globalization",
		defaultSeverity: DiagnosticSeverity.Warning,
		helpLinkUri: HelpLinkUri,
		isEnabledByDefault: true
	);

	private static readonly DiagnosticDescriptor DuplicateKeyWarning = new(
		id: "ResX004",
		title: "Duplicate Key in resource file",
		messageFormat: "Entry with key '{0}' is duplicated and will be ignored",
		category: "Globalization",
		defaultSeverity: DiagnosticSeverity.Warning,
		helpLinkUri: HelpLinkUri,
		isEnabledByDefault: true
	);

	private static readonly DiagnosticDescriptor MissingTranslationKeyWarning = new(
		id: "ResX005",
		title: "Missing translation for specific Key",
		messageFormat: "Entry with key '{0}' is missing a translation for {1} ({2})",
		category: "Globalization",
		defaultSeverity: DiagnosticSeverity.Warning,
		helpLinkUri: HelpLinkUri,
		isEnabledByDefault: true
	);

	public static bool TryGenerateSource(
		ResourceCollection resourceCollection,
		in List<Diagnostic> diagnostics,
		[NotNullWhen(true)] out string? sourceCode,
		CancellationToken cancellationToken
	)
	{
		ResourceInformation resourceInformation = resourceCollection.BaseInformation;

		GenerateNamespaceStartAndEnd(
			resourceInformation.Settings.CustomNamespace,
			out var namespaceStart,
			out var classIndent,
			out var memberIndent,
			out var namespaceEnd
		);
		if (
			!TryGenerateMembers(
				resourceCollection,
				memberIndent,
				out var valueMembers,
				out var nameMembers,
				out var keyMembers,
				diagnostics,
				cancellationToken
			)
		)
		{
			sourceCode = null;
			return false;
		}

		string? getStringMethod = null;
		if (resourceInformation.Settings.EmitFormatMethods)
		{
			getStringMethod += $$$$"""
{{{{memberIndent}}}}public string GetString(string name, string[]? formatterNames)
{{{{memberIndent}}}}{
{{{{memberIndent}}}}    var value = GetString(name);
{{{{memberIndent}}}}    if (formatterNames == null) return value;
{{{{memberIndent}}}}    for (var i = 0; i < formatterNames.Length; i++)
{{{{memberIndent}}}}    {
{{{{memberIndent}}}}        value = value.Replace($"{{{formatterNames[i]}}}", $"{{{i}}}");
{{{{memberIndent}}}}    }
{{{{memberIndent}}}}    return value;
{{{{memberIndent}}}}}

""";
		}

		var defaultClass = $$"""
{{classIndent}}/// <summary>A strongly typed resource class for '{{resourceInformation
				.ResourceFile
				.Path}}'.</summary>
{{classIndent}}{{(
				resourceInformation.Settings.Public ? "public" : "internal"
			)}} sealed partial class {{resourceInformation.ClassName}}
{{classIndent}}{
{{memberIndent}}private static {{resourceInformation.ClassName}}? _default;
{{memberIndent}}/// <summary>All values in <see cref="{{resourceInformation.ClassName}}"/> as the default instance of this class.</summary>
{{memberIndent}}public static {{resourceInformation.ClassName}} Values => _default ??= new {{resourceInformation.ClassName}}();

{{memberIndent}}/// <summary>Gets a value using a name, like a dictionary.</summary>
{{memberIndent}}/// <param name="name">The name of the key to retrieve the value.</param>
{{memberIndent}}/// <returns>The localized value.</returns>
{{memberIndent}}public string this[string name] => GetString(name);

{{memberIndent}}/// <summary>Gets a value using a key, like a dictionary.</summary>
{{memberIndent}}/// <param name="key">The key to retrieve the value.</param>
{{memberIndent}}/// <returns>The localized value.</returns>
{{memberIndent}}public string this[Keys key] => GetString(key.ToString());

{{memberIndent}}public delegate void CultureChangedDelegate(global::System.Globalization.CultureInfo? oldCulture, global::System.Globalization.CultureInfo? newCulture);
{{memberIndent}}/// <summary>Called after the <see cref="Culture"/> was updated.
{{memberIndent}}/// Provides previous culture and the newly set culture.</summary>
{{memberIndent}}public event CultureChangedDelegate? CultureChanged;

{{memberIndent}}private global::System.Globalization.CultureInfo? _culture;
{{memberIndent}}/// <summary>Gets or sets the culture to be used for all resource lookups issued by this strongly typed resource class.</summary>
{{memberIndent}}public System.Globalization.CultureInfo? Culture
{{memberIndent}}{
{{memberIndent}}    get => _culture;
{{memberIndent}}    set
{{memberIndent}}    {
{{memberIndent}}        System.Globalization.CultureInfo? oldCulture = _culture;
{{memberIndent}}        _culture = value;
{{memberIndent}}        if (!System.Collections.Generic.EqualityComparer<System.Globalization.CultureInfo>.Default.Equals(oldCulture, value))
{{memberIndent}}            CultureChanged?.Invoke(oldCulture, value);
{{memberIndent}}    }
{{memberIndent}}}

{{memberIndent}}///<summary>Returns the cached ResourceManager instance used by this class.</summary>
{{memberIndent}}[global::System.ComponentModel.EditorBrowsable(global::System.ComponentModel.EditorBrowsableState.Advanced)]
{{memberIndent}}public global::System.Resources.ResourceManager ResourceManager { get; } = new global::System.Resources.ResourceManager("{{resourceInformation.ResourceName}}", typeof({{resourceInformation.ClassName}}).Assembly);

{{memberIndent}}///<summary>Returns the cached ResourceManager instance for the host application, used for overrides.</summary>
{{memberIndent}}[global::System.ComponentModel.EditorBrowsable(global::System.ComponentModel.EditorBrowsableState.Advanced)]
{{memberIndent}}public global::System.Resources.ResourceManager? HostResourceManager => hostResourceManagerLazy.Value;

{{memberIndent}}private static readonly global::System.Lazy<global::System.Resources.ResourceManager?> hostResourceManagerLazy = new global::System.Lazy<global::System.Resources.ResourceManager?>(() =>
{{memberIndent}}{
{{memberIndent}}    var hostAssembly = global::System.Reflection.Assembly.GetEntryAssembly() ?? global::System.Reflection.Assembly.GetExecutingAssembly();
{{memberIndent}}    var hostResourceBaseName = hostAssembly.GetName().Name + ".{{resourceInformation.Settings.RelativeDir?.TrimEnd('\\') ?? "Resources"}}.{{resourceInformation.ResourceName}}";
{{memberIndent}}
{{memberIndent}}    if (hostAssembly.GetManifestResourceNames().Contains(hostResourceBaseName + ".resources"))
{{memberIndent}}    {
{{memberIndent}}        var hostResourceManager = new global::System.Resources.ResourceManager(hostResourceBaseName, hostAssembly);
{{memberIndent}}        if (hostResourceManager.GetResourceSet(global::System.Globalization.CultureInfo.InvariantCulture, true, true) == null) hostResourceManager = null;
{{memberIndent}}        return hostResourceManager;
{{memberIndent}}    }
{{memberIndent}}
{{memberIndent}}    return null;
{{memberIndent}}});

{{memberIndent}}/// <summary>Gets a resource of the <see cref="ResourceManager"/> with the configured <see cref="Culture"/> as a string.</summary>
{{memberIndent}}/// <param name="name">The name of the resource to get.</param>
{{memberIndent}}/// <returns>Returns the resource value as a string or the <paramref name="name"/> if it could not be found.</returns>
{{memberIndent}}[global::System.Runtime.CompilerServices.MethodImpl(global::System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
{{memberIndent}}public string GetString(string name)
{{memberIndent}}{
{{memberIndent}}    try
{{memberIndent}}    {
{{memberIndent}}        return HostResourceManager?.GetString(name, Culture) ?? ResourceManager.GetString(name, Culture) ?? name;
{{memberIndent}}    }
{{memberIndent}}    catch(global::System.Resources.MissingManifestResourceException) { }
{{memberIndent}}
{{memberIndent}}    return ResourceManager.GetString(name, Culture) ?? name;
{{memberIndent}}}

{{memberIndent}}/// <summary>Gets a resource of the <see cref="ResourceManager"/> with the configured <see cref="Culture"/> as a string.</summary>
{{memberIndent}}/// <param name="key">The key of the resource to get.</param>
{{memberIndent}}/// <returns>Returns the resource value as a string or the <paramref name="key"/> if it could not be found.</returns>
{{memberIndent}}[global::System.Runtime.CompilerServices.MethodImpl(global::System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
{{memberIndent}}public string GetString(Keys key) => GetString(key.ToString());

{{memberIndent}}/// <summary>Gets a value using the default instance.</summary>
{{memberIndent}}/// <param name="name">The name of the name to retrieve the value.</param>
{{memberIndent}}/// <returns>The localized value.</returns>
{{memberIndent}}[global::System.Runtime.CompilerServices.MethodImpl(global::System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
{{memberIndent}}public static string GetValue(string name) => Values.GetString(name);

{{memberIndent}}/// <summary>Gets a value using the default instance.</summary>
{{memberIndent}}/// <param name="key">The key to retrieve the value.</param>
{{memberIndent}}/// <returns>The localized value.</returns>
{{memberIndent}}[global::System.Runtime.CompilerServices.MethodImpl(global::System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)]
{{memberIndent}}public static string GetValue(Keys key) => Values.GetString(key);

{{getStringMethod}}
{{valueMembers}}
{{memberIndent}}/// <summary>All names in <see cref="{{resourceInformation.ClassName}}"/> as constants.</summary>
{{memberIndent}}public static class Names
{{memberIndent}}{
{{nameMembers}}
{{memberIndent}}}
{{memberIndent}}
{{memberIndent}}/// <summary>All keys in <see cref="{{resourceInformation.ClassName}}"/> as enums.</summary>
{{memberIndent}}public enum Keys
{{memberIndent}}{
{{keyMembers}}
{{memberIndent}}}
{{classIndent}}}
""";
		var debugInformation = resourceCollection.GenerateDebugInformation();
		var result = $"""
// <auto-generated/>
{debugInformation}
#nullable enable

{namespaceStart}
{defaultClass}
{namespaceEnd}

""";
		sourceCode = result.Replace("\r\n", "\n");
		return true;
	}

	private static string? GenerateDebugInformation(this ResourceCollection resourceCollection)
	{
		ResourceInformation resourceInformation = resourceCollection.BaseInformation;
		if (!resourceInformation.Settings.EmitDebugInformation)
			return null;
		return $"""
//
// Files:
// - {resourceCollection.FileHintName}
// - {resourceInformation.Settings.DefaultLang}: {resourceCollection.BaseInformation.ResourceFile.Path}
{string.Join("\n", resourceCollection.OtherLanguages.Select(x => $"// - {x.Key}: {x.Value.Path}"))}
//
// Configuration:
// - Public: {resourceInformation.Settings.Public}
// - ClassName: {(string.IsNullOrEmpty(resourceInformation.Settings.ClassName) ? resourceInformation.ClassName : resourceInformation.Settings.ClassName)}
// - DefaultLang: {resourceInformation.Settings.DefaultLang ?? "Default"}
// - RelativeDir: {resourceInformation.Settings.RelativeDir ?? "<null>"}
// - RootNamespace: {resourceInformation.Settings.RootNamespace ?? resourceInformation.Namespace ?? "<null>"}
// - CustomNamespace: {resourceInformation.Settings.CustomNamespace ?? resourceInformation.Namespace ?? "<null>"}
// - EmitFormatMethods: {resourceInformation.Settings.EmitFormatMethods}
//
// Computed properties:
// - ClassName: {resourceInformation.ClassName}
// - Namespace: {resourceInformation.Namespace ?? "<null>"}
// - ResourceName: {resourceInformation.ResourceName}
// - HostLookupPath: {resourceInformation.Settings.RelativeDir?.TrimEnd('\\') ?? "Resources"}\{resourceInformation.ResourceName}
//
""";
	}

	private static bool TryGenerateMembers(
		ResourceCollection resourceCollection,
		string memberIndent,
		[NotNullWhen(true)] out string? valueMembers,
		[NotNullWhen(true)] out string? nameMembers,
		[NotNullWhen(true)] out string? keyMembers,
		in List<Diagnostic> diagnostics,
		CancellationToken cancellationToken
	)
	{
		ResourceInformation resourceInformation = resourceCollection.BaseInformation;
		var valueMembersBuilder = new StringBuilder();
		var nameMembersBuilder = new StringBuilder();
		var keyMembersBuilder = new StringBuilder();

		if (
			!resourceInformation.ResourceFile.TryGetResourceDataAndValues(
				diagnostics,
				out Dictionary<string, XElement>? values,
				cancellationToken
			)
		)
		{
			valueMembers = nameMembers = keyMembers = null;
			return false;
		}

		if (values.Count == 0)
		{
			valueMembers = nameMembers = keyMembers = null;
			diagnostics.Add(
				Diagnostic.Create(
					descriptor: EmptyWarning,
					location: Location.Create(resourceInformation.ResourceFile.Path, default, default),
					messageArgs: null
				)
			);
			return false;
		}

		Dictionary<CultureInfo, Dictionary<string, XElement>> otherCulturesEntries = [];
		foreach (KeyValuePair<CultureInfo, AdditionalText> pair in resourceCollection.OtherLanguages)
		{
			if (
				!pair.Value.TryGetResourceDataAndValues(
					diagnostics,
					out Dictionary<string, XElement>? langSpecificValues,
					cancellationToken
				)
			)
			{
				continue;
			}

			otherCulturesEntries[pair.Key] = langSpecificValues;
		}

		foreach (KeyValuePair<string, XElement> x in values)
		{
			(var name, XElement attribute) = (x.Key, x.Value);
			var value = attribute.Value.Trim();
			var propertyIdentifier = GetIdentifierFromResourceName(name);

			valueMembersBuilder.AppendLine(
				$"""
{memberIndent}/// <summary>Gets the value of <see cref="Keys.@{propertyIdentifier}"/>.<list type="table">
{memberIndent}/// <item><b>{resourceInformation.Settings.DefaultLang}</b>: {GetTrimmedDocComment(
					"description",
					value
				)}</item>
"""
			);

			nameMembersBuilder.AppendLine(
				$"""
{memberIndent}    /// <summary>Gets the name of <see cref="Keys.@{propertyIdentifier}"/>.<list type="table">
{memberIndent}    /// <item><b>{resourceInformation.Settings.DefaultLang}</b>: {GetTrimmedDocComment(
					"description",
					value
				)}</item>
"""
			);

			keyMembersBuilder.AppendLine(
				$"""
{memberIndent}    /// <summary>Gets the key of <see cref="Keys.@{propertyIdentifier}"/>.<list type="table">
{memberIndent}    /// <item><b>{resourceInformation.Settings.DefaultLang}</b>: {GetTrimmedDocComment(
					"description",
					value
				)}</item>
"""
			);

			foreach (
				KeyValuePair<CultureInfo, Dictionary<string, XElement>> entry in otherCulturesEntries.OrderBy(item =>
					item.Key.ToString()
				)
			)
			{
				string otherValue;
				if (entry.Value.TryGetValue(name, out XElement otherAttribute))
				{
					otherValue = otherAttribute.Value.Trim();
				}
				else
				{
					diagnostics.Add(
						Diagnostic.Create(
							descriptor: MissingTranslationKeyWarning,
							location: Location.Create(
								resourceCollection.OtherLanguages[entry.Key].Path,
								default,
								default
							),
							messageArgs: [name, entry.Key.DisplayName, entry.Key]
						)
					);
					otherValue = "[missing]";
				}

				valueMembersBuilder.AppendLine(
					$"{memberIndent}/// <item><b>{entry.Key}</b>: {GetTrimmedDocComment("description", otherValue)}</item>"
				);

				nameMembersBuilder.AppendLine(
					$"{memberIndent}    /// <item><b>{entry.Key}</b>: {GetTrimmedDocComment("description", otherValue)}</item>"
				);

				keyMembersBuilder.AppendLine(
					$"{memberIndent}    /// <item><b>{entry.Key}</b>: {GetTrimmedDocComment("description", otherValue)}</item>"
				);

			}

			valueMembersBuilder.AppendLine(
				$"""
{memberIndent}/// <item><description>⠀</description></item>
{memberIndent}/// </list></summary>
{memberIndent}public string @{propertyIdentifier} => GetString(Keys.@{propertyIdentifier});
"""
			);

			if (resourceInformation.Settings.EmitFormatMethods)
			{
				var resourceString = new ResourceString(propertyIdentifier, value);
				if (resourceString.HasArguments)
				{
					RenderFormatMethod(memberIndent, valueMembersBuilder, resourceString);
				}
			}

			nameMembersBuilder.AppendLine(
				$"""
{memberIndent}    /// </list></summary>
{memberIndent}    public const string @{propertyIdentifier} = @"{name}";
"""
			);

			keyMembersBuilder.AppendLine(
				$"""
{memberIndent}    /// </list></summary>
{memberIndent}    @{propertyIdentifier},
"""
			);
		}

		valueMembers = valueMembersBuilder.ToString();
		nameMembers = nameMembersBuilder.ToString();
		keyMembers = keyMembersBuilder.ToString().TrimEnd();
		return true;
	}

	private static void RenderFormatMethod(string indent, StringBuilder strings, ResourceString resourceString)
	{
		var propertyIdentifier = resourceString.Identifier;
		var methodParameters = resourceString.GetMethodParameters();
		var arguments = resourceString.GetJoinedArguments();
		var argumentNames = resourceString.UsingNamedArgs
			? $"GetString(@{propertyIdentifier}, new[] {{ {resourceString.GetArgumentNames()} }})"
			: $"@{propertyIdentifier}";
		var paramDocs = string.Join(
			"\n",
			resourceString
				.GetArguments()
				.Select(
					(x, i) => $"{indent}/// <param name=\"{x}\">The parameter to be used at position {{{i}}}.</param>"
				)
		);

		strings.AppendLine(
			$"""
{indent}/// <summary>Formats the content of <see cref="Keys.@{propertyIdentifier}"/>.</summary>
{indent}/// {GetTrimmedDocComment("value", resourceString.Value)}
{paramDocs}
{indent}/// <returns>The formatted <see cref="Keys.@{propertyIdentifier}"/> content.</returns>
{indent}public string @Format{propertyIdentifier}({methodParameters}) => string.Format(Culture, {argumentNames}, {arguments});
"""
		);
	}

	private static bool TryGetResourceDataAndValues(
		this AdditionalText additionalText,
		in List<Diagnostic> diagnostics,
		[NotNullWhen(true)] out Dictionary<string, XElement>? resourceNames,
		CancellationToken cancellationToken
	)
	{
		SourceText? text = additionalText.GetText(cancellationToken);
		if (text is null)
		{
			diagnostics.Add(
				Diagnostic.Create(
					descriptor: EmptyWarning,
					location: Location.Create(additionalText.Path, default, default),
					messageArgs: null
				)
			);
			resourceNames = null;
			return false;
		}

		using var sourceTextReader = new SourceTextReader(text);
		resourceNames = [];
		foreach (XElement node in XDocument.Load(sourceTextReader, LoadOptions.SetLineInfo).Descendants("data"))
		{
			XAttribute? nameAttribute = node.Attribute("name");
			var name = nameAttribute?.Value;
			if (nameAttribute is null || name is null || string.IsNullOrWhiteSpace(name))
			{
				diagnostics.Add(
					Diagnostic.Create(
						descriptor: InvalidKeyWarning,
						location: GetXElementLocation(additionalText, node, name),
						messageArgs: name
					)
				);
				continue;
			}

			XElement? valueAttribute = node.Elements("value").FirstOrDefault();
			if (valueAttribute is null)
			{
				diagnostics.Add(
					Diagnostic.Create(
						descriptor: MissingValueWarning,
						location: GetXElementLocation(additionalText, nameAttribute, name),
						messageArgs: name
					)
				);
				continue;
			}

			if (resourceNames.ContainsKey(name))
			{
				diagnostics.Add(
					Diagnostic.Create(
						descriptor: DuplicateKeyWarning,
						location: GetXElementLocation(additionalText, nameAttribute, name),
						messageArgs: name
					)
				);
				continue;
			}

			resourceNames[name] = valueAttribute;
		}

		return true;
	}

	private static Location GetXElementLocation(AdditionalText text, IXmlLineInfo line, string? memberName) =>
		Location.Create(
			filePath: text.Path,
			textSpan: new TextSpan(),
			lineSpan: new LinePositionSpan(
				start: new LinePosition(line.LineNumber - 1, line.LinePosition - 1),
				end: new LinePosition(line.LineNumber - 1, line.LinePosition - 1 + memberName?.Length ?? 0)
			)
		);

	private static string GetTrimmedDocComment(string elementName, string value)
	{
		var trimmedValue = value.Length > MaxDocCommentLength ? value[..MaxDocCommentLength] + " ..." : value;
		var element = new XElement(elementName, trimmedValue).ToString();
		var splits = element.Split('\n');
		return string.Join("<br/>", splits.Select(x => x.Trim()));
	}

	private static void GenerateNamespaceStartAndEnd(
		string? namespaceName,
		out string? namespaceStart,
		out string classIndent,
		out string memberIndent,
		out string? namespaceEnd
	)
	{
		const string indent = "    ";
		if (namespaceName is null)
		{
			namespaceStart = null;
			classIndent = "";
			namespaceEnd = null;
		}
		else
		{
			namespaceStart = $$"""
namespace {{namespaceName}}
{
""";
			classIndent = indent;
			namespaceEnd = "}";
		}

		memberIndent = classIndent + indent;
	}

	public static bool SplitName(string fullName, [NotNullWhen(true)] out string? namespaceName, out string className)
	{
		var lastDot = fullName.LastIndexOf('.');
		if (lastDot == -1)
		{
			namespaceName = null;
			className = fullName;
			return false;
		}

		namespaceName = fullName[..lastDot];
		className = fullName[(lastDot + 1)..];
		return true;
	}

	public delegate bool TryParseDelegate<T>(string value, [NotNullWhen(true)] out T? result)
		where T : class;
	public delegate bool TryParseDelegateStruct<T>(string value, [NotNullWhen(true)] out T result)
		where T : struct;

	public static bool? GetBoolValue(this AnalyzerConfigOptions options, string key) =>
		options.GetStructValue(key, (string value, out bool result) => bool.TryParse(value, out result));

	public static T? GetStructValue<T>(
		this AnalyzerConfigOptions options,
		string key,
		TryParseDelegateStruct<T> tryParse
	)
		where T : struct
	{
		if (options.TryGetValue(key, out var stringValue) && tryParse(stringValue, out T value))
		{
			return value;
		}

		return null;
	}

	public static string? GetValue(this AnalyzerConfigOptions options, string key) =>
		options.TryGetValue(key, out var stringValue) ? stringValue : null;

	internal static bool IsChildFile(
		string fileToCheck,
		IEnumerable<string> availableFiles,
		[NotNullWhen(true)] out CultureInfo? cultureInfo
	)
	{
		SplitName(fileToCheck, out var parentFileName, out var languageExtension);
		if (!availableFiles.Contains(parentFileName))
		{
			cultureInfo = null;
			return false;
		}

		var lastNumberOfCodes = 0;
		var sections = 0;
		foreach (var character in languageExtension)
		{
			switch (character)
			{
				case '-' when lastNumberOfCodes < 2 || sections > 1:
					cultureInfo = null;
					return false;
				case '-':
					lastNumberOfCodes = 0;
					sections++;
					continue;
				default:
					lastNumberOfCodes++;
					break;
			}
		}

		if (lastNumberOfCodes is > 4 or < 2)
		{
			cultureInfo = null;
			return false;
		}

		try
		{
			cultureInfo = CultureInfo.GetCultureInfo(languageExtension);
		}
		catch (CultureNotFoundException)
		{
			cultureInfo = null;
			return false;
		}

		return true;
	}

	public static string GetIdentifierFromResourceName(string name)
	{
		if (name.All(CharExtensions.IsIdentifierPartCharacter))
		{
			return name[0].IsIdentifierStartCharacter() ? name : "_" + name;
		}

		var builder = new StringBuilder(name.Length);

		var f = name[0];
		if (f.IsIdentifierPartCharacter() && !f.IsIdentifierStartCharacter())
		{
			builder.Append('_');
		}

		foreach (var c in name)
		{
			builder.Append(c.IsIdentifierPartCharacter() ? c : '_');
		}

		return builder.ToString();
	}

	private readonly struct ResourceString
	{
		private static readonly Regex NamedParameterMatcher = new(
			@"\{([a-z]\w*)\}",
			RegexOptions.IgnoreCase | RegexOptions.Compiled
		);
		private static readonly Regex NumberParameterMatcher = new(@"\{(\d+)\}", RegexOptions.Compiled);
		private readonly IReadOnlyList<string> _arguments;

		public ResourceString(string identifier, string value)
		{
			Identifier = identifier;
			Value = value;

			MatchCollection match = NamedParameterMatcher.Matches(value);
			UsingNamedArgs = match.Count > 0;

			if (!UsingNamedArgs)
			{
				match = NumberParameterMatcher.Matches(value);
			}

			IEnumerable<string> arguments = match.Cast<Match>().Select(m => m.Groups[1].Value).Distinct();
			if (!UsingNamedArgs)
			{
				arguments = arguments.OrderBy(Convert.ToInt32);
			}

			_arguments = arguments.ToList();
		}

		public string Identifier { get; }
		public string Value { get; }

		public bool UsingNamedArgs { get; }

		public bool HasArguments => _arguments.Count > 0;

		public string GetArgumentNames() => string.Join(", ", _arguments.Select(a => "\"" + a + "\""));

		public IEnumerable<string> GetArguments()
		{
			var usingNamedArgs = UsingNamedArgs;
			return _arguments.Select(s => GetArgName(s, usingNamedArgs));
		}

		public string GetJoinedArguments() => string.Join(", ", GetArguments());

		public string GetMethodParameters()
		{
			var usingNamedArgs = UsingNamedArgs;
			return string.Join(", ", _arguments.Select(a => "object? " + GetArgName(a, usingNamedArgs)));
		}

		private static string GetArgName(string name, bool usingNamedArgs) => usingNamedArgs ? name : 'v' + name;
	}
}
