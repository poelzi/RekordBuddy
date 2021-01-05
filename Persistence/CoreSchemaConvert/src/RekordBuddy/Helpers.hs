{-# LANGUAGE OverloadedStrings #-}
module RekordBuddy.Helpers where
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (ap)
import Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.String as RT
import Numeric (showHex)
import Data.Char (isAscii, isPrint, ord)

data Flag = ConstF | VirtualF | StaticF | OverrideF | ExternF
   deriving (Show, Eq, Ord)

-- | Put all of the Flag constants onto the document in the correct place (order dependent)
flagAffix :: Doc ann -> [Flag] -> Doc ann
flagAffix = foldr (ap binopForFlag docForFlag)

-- Given a list of lines, put them together with blank lines separating them
doubleLined :: [Doc ann] -> Doc ann
doubleLined = vsep . punctuate line

vsepMap :: (Doc ann -> Doc ann) -> [Doc ann] -> Doc ann
vsepMap f = vsep . map f

vcatMap :: (Doc ann -> Doc ann) -> [Doc ann] -> Doc ann
vcatMap f = vcat . map f

vsepCat :: [[Doc ann]] -> Doc ann
vsepCat = vsep . map vcat

($+$) :: Doc ann -> Doc ann -> Doc ann
a $+$ b = vsep [a, b]

-- | Declare a C++ method
methodDecl :: Doc ann -> [Flag] -> [Doc ann] -> Doc ann -> Doc ann
methodDecl name flags params ret =
   flagAffix (ret <+> call name params) flags

-- | Define a method (signature and body) with some nnesting for inner statements
methodDef :: Int -> Doc ann -> [Flag] -> [Doc ann] -> Doc ann -> [Doc ann] -> Doc ann
methodDef nnesting name flags params ret =
   hangInMethod nnesting (methodDecl name flags params ret)

-- | Define a variable using C++ constructor syntax. Will be emplace-constructed
variableDef :: Doc ann -> Doc ann -> [Doc ann] -> Doc ann
variableDef t name value =
   statement ((t <+> name) <+> braceList value)

autoVarDef :: Doc ann -> Doc ann -> Doc ann
autoVarDef a v = variableDef "auto" a [v]

autoInit :: Doc ann -> Doc ann -> Doc ann
autoInit a = assign ("auto" <+> a)

-- | init-and-test to scope auto var = iexp; if (<<f var>>) {body;}
autoInitTestScope :: Int -> Doc ann -> (Doc ann -> Doc ann) -> Doc ann -> [Doc ann] -> Doc ann
autoInitTestScope nnesting var f iexp body = vcat
   [ autoInit var iexp
   , guardP nnesting (f var) body
   ]

data ConstructorStatus = EqualsDelete | EqualsDefault

cstatusP :: ConstructorStatus -> Doc ann
cstatusP EqualsDelete = "delete"
cstatusP EqualsDefault = "default"

defaultCopyAssignDef, defaultMoveCtorDef, defaultDestructDef, defaultVirtDestructDef, defaultCopyCtorDef :: ConstructorStatus -> Doc ann -> Doc ann
defaultCopyCtorDef c name     = statement (name <> parens ("const" <+> name <> "&") /=/ cstatusP c)
defaultVirtDestructDef c name = statement ("virtual" <+> "~" <> name <> parens mempty /=/ cstatusP c)
defaultDestructDef c name     = statement ("~" <> name <> parens mempty /=/ cstatusP c)
defaultMoveCtorDef c name     = statement (name <> parens (name <> "&&") /=/ cstatusP c)
defaultCopyAssignDef c name   = statement (reference name <+> "operator=" <> parens ("const" <+> name <> "&") /=/ cstatusP c)

conditionalCompile :: Doc ann -> [Doc ann] -> Maybe [Doc ann] -> Doc ann
conditionalCompile condition body Nothing =
   vcat (["#if defined(" <+> condition <+> ")"] ++ body ++ ["#endif"])
conditionalCompile condition [] (Just elsebody) =
   vcat (["#if !defined(" <+> condition <+> ")"] ++ elsebody ++ ["#endif"])
conditionalCompile condition thenbody (Just elsebody) =
   vcat (["#if defined(" <+> condition <+> ")"] ++ thenbody ++ ["#else"] ++ elsebody ++ ["#endif"])

-- | Send an objective C method. Arguments are joined with objcSepArgs.
objcSend :: Doc ann -> [Doc ann] -> Doc ann
objcSend dest args = brackets (dest <+> sep (objcSepArgs args))

-- | performs the objc-style argument style. Joined pairwise with colons and then by space
objcSepArgs :: [Doc ann] -> [Doc ann]
objcSepArgs (a:b:rest) = (a <> colon <> b) : objcSepArgs rest
objcSepArgs []         = []
objcSepArgs [a]        = [a]

-- | Declare a member method (no body, no semi)
objcClassMethodDecl :: Doc ann -> [Doc ann] -> Doc ann
objcClassMethodDecl returnType nameParams =
   "+" <+> parens returnType <> sep (objcSepArgs nameParams)

-- | Define a method, including body
objcClassMethodDef :: Int -> Doc ann -> [Doc ann] -> [Doc ann] -> Doc ann
objcClassMethodDef nnesting returnType nameParams =
   hangInMethod nnesting (objcClassMethodDecl returnType nameParams)


-- | Declare a member method (no body, no semi)
objcMemberMethodDecl :: Doc ann -> [Doc ann] -> Doc ann
objcMemberMethodDecl returnType nameParams =
   "-" <+> parens returnType <> sep (objcSepArgs nameParams)

-- | Define a method, including body
objcMemberMethodDef :: Int -> Doc ann -> [Doc ann] -> [Doc ann] -> Doc ann
objcMemberMethodDef nnesting returnType nameParams =
   hangInMethod nnesting (objcMemberMethodDecl returnType nameParams)

objcSelector :: Doc ann -> Doc ann
objcSelector doc = "@selector" <> parens doc

-- | renders a prefix, a name and a group of inner docs
objcAtBlock :: Doc ann -> Doc ann -> [Doc ann] -> Doc ann
objcAtBlock prefix name body = vsep
   (( "@" <> prefix <+> name) : body ++ ["@end"])

objcInterface :: Doc ann -> [Doc ann] -> Doc ann
objcInterface = objcAtBlock "interface"

objcImplementation :: Doc ann -> [Doc ann] -> Doc ann
objcImplementation = objcAtBlock "implementation"

-- | Quite a generic helper that just produces a comma separated list.
commaList :: [Doc ann] -> Doc ann
commaList args = group (align (vsep $ punctuate comma args))

angleList :: [Doc ann] -> Doc ann
angleList args = angles (commaList args)

template :: Doc ann -> [Doc ann] -> Doc ann
template thing args = thing <> angleList args

templateScope :: [Doc ann] -> Doc ann -> Doc ann
templateScope args body = nest 4 (vcat [template "template" args, body])

-- | Enclose a commaList in brackets
bracketList :: [Doc ann] -> Doc ann
bracketList args = brackets (commaList args)

-- | Enclose a commaList in braces
braceList :: [Doc ann] -> Doc ann
braceList args = braces (commaList args)

-- | Enclose a commaList in parens
parenCommaList :: [Doc ann] -> Doc ann
parenCommaList args = parens (commaList args)

lambdaP :: Int -> [Doc ann] -> [Doc ann] -> [Doc ann] -> Doc ann
lambdaP nnesting binds args = hangInScope nnesting (brackets (commaList binds) <> parens (commaList args))

-- | std::tuple of n items
tupleOf :: [Doc ann] -> Doc ann
tupleOf a = "std"/::/"tuple" <> angleList a

stdBegin, stdMove, stdEnd :: Doc ann -> Doc ann
stdBegin from = call ("std"/::/"begin") [from]
stdEnd from = call ("std"/::/"end") [from]
stdMove from = call ("std"/::/"move") [from]

stdGet :: Doc ann -> Doc ann -> Doc ann
stdGet field from = call ("std"/::/"get" <> angleList [field]) [from]

uniquePtrOf :: Doc ann -> Doc ann
uniquePtrOf t = "std"/::/"unique_ptr" <> angleList [t]

makeUnique :: [Doc ann] -> [Doc ann] -> Doc ann
makeUnique = call . template ("std" /::/ "make_unique")

sharedPtrOf :: Doc ann -> Doc ann
sharedPtrOf t = "std"/::/"shared_ptr" <> angleList [t]

makeShared :: [Doc ann] -> [Doc ann] -> Doc ann
makeShared = call . template ("std" /::/ "make_shared")

staticCastSharedPtr :: Doc ann -> Doc ann -> Doc ann
staticCastSharedPtr t val = call ("std"/::/"static_pointer_cast" <> angleList [t]) [val]

maybeDoc :: Maybe (Doc ann) -> Doc ann
maybeDoc (Just d) = d
maybeDoc Nothing = mempty

-- | Call a method in the C++ style. name(args)
call :: Doc ann -> [Doc ann] -> Doc ann
call name args = name <> parenCommaList args

callS :: Doc ann -> [Doc ann] -> Doc ann
callS a b = statement (call a b)

initialize :: Doc ann -> [Doc ann] -> Doc ann
initialize name args = name <> braceList args

callStmt :: Doc ann -> [Doc ann] -> Doc ann
callStmt name args = statement (call name args)

-- | Hang statements with either a trailing lbrace or a lbrace on new line, a nested body, and an unindented rbrace.
hangInStmts :: Bool -> Int -> Doc ann -> [Doc ann] -> Doc ann
hangInStmts True _ d1 [] = d1 <+> "{}"
hangInStmts braceSame nnesting d1 d2 =
   let fline body = if braceSame then d1 <+> nest nnesting (lbrace $+$ body) else (vsep [d1, nest nnesting (lbrace $+$ body)])
   in (fline (vsep d2)) $+$ rbrace

objcBlock :: Int -> Doc ann -> [Doc ann] -> Doc ann
objcBlock nnesting args body =
   let fline = args <> "^" <> lbrace
   in fline $+$ nest nnesting (vcat body) $+$ rbrace

-- | Hang methods and scopes, which vary only in whether the lbrace is on a newline
hangInMethod, hangInScope :: Int -> Doc ann -> [Doc ann] -> Doc ann
hangInMethod = hangInStmts False
hangInScope = hangInStmts True

-- pre C++17 namespaces have to be one-per-block. C++17 and higher allows `namespace A::B::C {}`
embedInNamespace :: [Text] -> Doc ann -> Doc ann
embedInNamespace ns as = namespace 0 False (scope (map dtext ns)) [as]

namespace :: Int -> Bool -> Doc ann -> [Doc ann] -> Doc ann
namespace nnesting inlined name = hangInScope nnesting ((if inlined then "inline namespace" else "namespace") <+> name)

usingNamespace :: Doc ann -> Doc ann
usingNamespace a = statement ("using" <+> "namespace" <+> a)

-- | `using` type alias. using X = Y;
using :: Doc ann -> Doc ann -> Doc ann
using a b = statement ("using" <+> a /=/ b)

staticCast :: Doc ann -> Doc ann -> Doc ann
staticCast t value = "static_cast" <> angleList [t] <> parens value

conditionalGuardP :: Int -> Bool -> Doc ann -> (Doc ann -> [Doc ann]) -> Doc ann
conditionalGuardP nnesting cond test body =
   if cond
   then ifP nnesting test (body (parens (deref test))) Nothing
   else vcat (body test)

guardP :: Int -> Doc ann -> [Doc ann] -> Doc ann
guardP nnesting test body = ifP nnesting test body Nothing

-- | Generic if with optional else.
ifP :: Int -> Doc ann -> [Doc ann] -> Maybe [Doc ann] -> Doc ann
ifP nnesting cond thn Nothing =
   hangInScope nnesting ("if" <+> parens cond) thn
ifP nnesting cond thn (Just els) =
   ifP nnesting cond thn Nothing $+$ hangInScope nnesting "else" els

switchP :: Int -> Doc ann -> [Doc ann] -> Doc ann
switchP nnesting test = hangInScope nnesting ("switch" <+> parens test)

whileP :: Int -> Doc ann -> [Doc ann] -> Doc ann
whileP nnesting test = hangInScope nnesting ("while" <+> parens test)

autoForP :: Int -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
autoForP nnesting var source =
   hangInScope nnesting ("for" <+> parens ("auto" <+> "&&" <+> var <+> ":" <+> source))

-- | C++ "classes" can be declared as structs too
data ClassType = Struct | Class

-- | Produce a doc out of a ClassType
printClassType :: ClassType -> Doc ann
printClassType Struct = struct
printClassType Class = klass

-- | Declare a class, including a body
classDecl :: Int -> ClassType -> Doc ann -> [Doc ann] -> [Doc ann] -> Doc ann
classDecl nnesting ct name [] body     = statement $ hangInMethod nnesting (printClassType ct <+> name) body
classDecl nnesting ct name derivs body = statement $ hangInMethod nnesting (printClassType ct <+> group (nest 4 (name $+$ (colon <+> commaList derivs)))) body

-- | Simple semicolon appendment
statement :: Doc ann -> Doc ann
statement = append semi

-- | Pragma mark for sections
pragmaMark :: Doc ann -> Doc ann
pragmaMark a = lineComment ("--" <+> a)

-- | Various keywords
public, private, protected, klass, struct, void, pragma, pragmaOnce :: Doc ann
overrideP, staticP, virtualP, externP, constP, returnP :: Doc ann
public     = "public"
private    = "private"
protected  = "protected"
klass      = "class"
struct     = "struct"
void       = "void"
pragma     = "#pragma"
overrideP  = "override"
staticP    = "static"
externP    = "extern"
virtualP   = "virtual"
constP     = "const"
returnP    = "return"
pragmaOnce = pragma <+> "once"

-- Two blank lines between documents
(%%) :: Doc ann -> Doc ann -> Doc ann
a %% b = vsep [a, b]

scope :: [Doc ann] -> Doc ann
scope args = hcat $ punctuate "::" args

prentext :: String -> Doc ann
prentext = parens . pretty

prepend, prependS, appendS, append :: Doc ann -> Doc ann -> Doc ann
prepend  = (<>)
prependS = (<+>)
append   = flip (<>)
appendS  = flip (<+>)

    {-
angles :: Doc ann -> Doc ann
angles = append (char '>') . prepend (char '<')
-}

typedef :: Doc ann -> Doc ann -> Doc ann
typedef a b       = "typedef" <+> a <+> b

assign :: Doc ann -> Doc ann -> Doc ann
returnD, deref, returnS, literalString, negateP, nsLiteralString, typename, ptrTo, reference, addressOf, constReference, constPtrTo :: Doc ann -> Doc ann
objcImport, objcImportSystem, include, includeSystem, lineComment, blockComment :: Doc ann -> Doc ann
returnDefault :: Doc ann
objcImport str       = "#import" <+> dquotes str
objcImportSystem str = "#import" <+> angles str
include str         = "#include" <+> dquotes str
includeSystem str   = "#include" <+> angles str
lineComment         = prependS "//"
blockComment str    = "/*" <+> str <+> "*/"
literalString str   = dquotes (cEscape (RT.renderString (layoutCompact str)))
nsLiteralString str = prepend "@" (literalString str)
returnD             = prependS "return"
returnS             = statement . returnD
returnDefault       = statement $ "return" <+> braceList []
typename            = prependS "typename"
assign a b          = statement (a /=/ b)
deref               = prepend "*"
addressOf           = prepend "&"
negateP             = prepend "!"
ptrTo               = append "*"
reference           = append "&"
constReference      = prependS constP . reference
constPtrTo          = prependS constP . ptrTo



-- | Attempt to make any given character safe for injecting into a quoted C string
cEscapeChar :: Char -> Doc ann
cEscapeChar '\n' = "\\n"
cEscapeChar '\t' = "\\t"
cEscapeChar '\a' = "\\a"
cEscapeChar '\b' = "\\b"
cEscapeChar '\f' = "\\f"
cEscapeChar '\r' = "\\r"
cEscapeChar '\v' = "\\v"
cEscapeChar '\\' = "\\\\"
cEscapeChar '\'' = "\\'"
cEscapeChar '\"' = "\\\""
cEscapeChar '?'  = "\\?"
cEscapeChar x | isAscii x && isPrint x = pretty x
cEscapeChar x = "\\u" <> pretty (showHex (ord x) "")

-- | Make any given string ok to inject into a doublequoted string
cEscape :: String -> Doc ann
cEscape = foldr ((<>) . cEscapeChar) mempty

-- | Make a Data.Text string able to be injected into a document
dtext :: Text -> Doc ann
dtext = pretty

-- | stick an unnested access specifier into the document
privateAccess, protectedAccess, publicAccess :: Int -> Doc ann
publicAccess nnesting    = nest (-1 * nnesting) (line <> public <> colon)
protectedAccess nnesting = nest (-1 * nnesting) (line <> protected <> colon)
privateAccess nnesting   = nest (-1 * nnesting) (line <> private <> colon)

ternaryIf :: Doc ann -> Doc ann -> Doc ann -> Doc ann
ternaryIf test a b =
   hsep [parens test, "?" <+>  parens a, ":" <+> parens b]

-- | Generic binary operator definition
binop :: (Doc ann -> Doc ann -> Doc ann) -> String -> Doc ann -> Doc ann -> Doc ann
binop op opname l r = l `op` pretty opname `op` r

(/->/), (/./),  (/&&/), (/||/),
 (/*/),
 (///),
 (/>/),
 (/+/),  (/-/),  (/=/),
 (/==/), (/!=/), (/&/),  (/|/), (/:/),
 (/<=/), (/</),  (/>>/), (/<</), (/::/) :: Doc ann -> Doc ann -> Doc ann

(/->/) = binop (<>)  "->"
(/./)  = binop (<>)  "."
(/&&/) = binop (<+>) "&&"
(/||/) = binop (<+>) "||"
(/*/)  = binop (<+>) "*" -- ugg, hlint can't parse because it looks like a c comment start. comment out to lint
(///)  = binop (<+>) "/"
(/+/)  = binop (<+>) "+"
(/-/)  = binop (<+>) "-"
(/=/)  = binop (<+>) "="
(/==/) = binop (<+>) "=="
(/!=/) = binop (<+>) "!="
(/&/)  = binop (<+>) "&"
(/|/)  = binop (<+>) "|"
(/<=/) = binop (<+>) "<="
(/</)  = binop (<+>) "<"
(/>/)  = binop (<+>) ">"
(/>>/) = binop (<+>) ">>"
(/<</) = binop (<+>) "<<"
(/:/)  = binop (<+>) ":"
(/::/) = binop (<>) "::"

infixr 8 /</
infixr 8 />/
infixr 8 /<=/
infixr 8 /==/
infixr 8 /!=/
infixr 7 /&&/
infixr 6 /||/
infixr 5 /-/
infixr 5 /+/
infixr 4 />>/
infixr 4 /<</
infixr 3 /&/
infixr 3 /|/
infixr 2 /*/
infixr 2 ///
infixr 1 /./
infixr 1 /:/
infixr 1 /::/
infixr 1 /->/

binopForFlag :: Flag -> Doc ann -> Doc ann -> Doc ann
binopForFlag ConstF    = appendS
binopForFlag VirtualF  = prependS
binopForFlag StaticF   = prependS
binopForFlag OverrideF = appendS
binopForFlag ExternF   = prependS

docForFlag :: Flag -> Doc ann
docForFlag ConstF      = constP
docForFlag VirtualF    = virtualP
docForFlag StaticF     = staticP
docForFlag OverrideF   = overrideP
docForFlag ExternF     = externP
