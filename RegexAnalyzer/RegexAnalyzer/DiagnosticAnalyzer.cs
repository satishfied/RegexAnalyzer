using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace RegexAnalyzer
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class RegexAnalyzerAnalyzer : DiagnosticAnalyzer
    {
        //public const string DiagnosticId = "RegexAnalyzer";

        // You can change these strings in the Resources.resx file. If you do not want your analyzer to be localize-able, you can use regular strings for Title and MessageFormat.
        // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Localizing%20Analyzers.md for more on localization
        //private static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        //private static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        //private static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof(Resources));
        //private const string Category = "Naming";

        public const string DiagnosticId = "Regex";
        internal const string Title = "Regex error parsing string argument";
        internal const string MessageFormat = "Regex error {0}";
        internal const string Description = "Regex patterns should be syntactically valid.";
        internal const string Category = "Syntax";

        //private static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, isEnabledByDefault: true, description: Description);
        internal static DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Error, isEnabledByDefault: true, description: Description);
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }

        public override void Initialize(AnalysisContext context)
        {
            // TODO: Consider registering other actions that act on syntax instead of or in addition to symbols
            // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Analyzer%20Actions%20Semantics.md for more information
            //context.RegisterSymbolAction(AnalyzeSymbol, SymbolKind.NamedType);
            context.RegisterSyntaxNodeAction(AnalyzeNode, SyntaxKind.InvocationExpression);
        }

        private static void AnalyzeNode(SyntaxNodeAnalysisContext context)
        {
            var invocationExpr = (InvocationExpressionSyntax)context.Node;
            var memberAccessExpr = invocationExpr.Expression as MemberAccessExpressionSyntax;
            if (memberAccessExpr?.Name.ToString() != "Match") return;
            var memberSymbol = context.SemanticModel.GetSymbolInfo(memberAccessExpr).Symbol as IMethodSymbol;
            if (!memberSymbol?.ToString().StartsWith("System.Text.RegularExpressions.Regex.Match") ?? true) return;
            var argumentList = invocationExpr.ArgumentList as ArgumentListSyntax;
            if ((argumentList?.Arguments.Count ?? 0) < 2) return;
            var regexLiteral = argumentList.Arguments[1].Expression as LiteralExpressionSyntax;
            if (regexLiteral == null) return;
            var regexOpt = context.SemanticModel.GetConstantValue(regexLiteral);
            if (!regexOpt.HasValue) return;
            var regex = regexOpt.Value as string;
            if (regex == null) return;

            try
            {
                System.Text.RegularExpressions.Regex.Match("", regex);
            }
            catch (ArgumentException e)
            {
                var diagnostic = Diagnostic.Create(Rule, regexLiteral.GetLocation(), e.Message);
                context.ReportDiagnostic(diagnostic);
            }
        }

        private static void AnalyzeSymbol(SymbolAnalysisContext context)
        {
            // TODO: Replace the following code with your own analysis, generating Diagnostic objects for any issues you find
            var namedTypeSymbol = (INamedTypeSymbol)context.Symbol;

            // Find just those named type symbols with names containing lowercase letters.
            if (namedTypeSymbol.Name.ToCharArray().Any(char.IsLower))
            {
                // For all such symbols, produce a diagnostic.
                var diagnostic = Diagnostic.Create(Rule, namedTypeSymbol.Locations[0], namedTypeSymbol.Name);

                context.ReportDiagnostic(diagnostic);
            }
        }
    }
}
