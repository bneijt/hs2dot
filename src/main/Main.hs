
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import System.FilePath
import System.Directory
import Control.Monad
import Data.Maybe
import Data.List
import Control.Applicative
import Data.GraphViz.Types.Graph
import Data.GraphViz.Commands.IO
import Data.GraphViz.Types.Canonical
import Data.Text.Lazy (pack)

relations :: Module -> (ModuleName, [ModuleName])
relations (Module _ name _ _ _ imports _) = (name, map importModule imports)

findHsFiles :: String -> IO [String]
findHsFiles path = do
    allFiles <- getDirectoryContents path
    let files = map (path </>) $ filter (flip notElem $ [".", ".."]) allFiles
    let hsFiles = filter (\x -> takeExtension x == ".hs") files
    directories <- filterM doesDirectoryExist files
    ((hsFiles ++) . concat) <$> mapM findHsFiles directories

relationsFrom :: FilePath -> IO (Maybe (ModuleName, [ModuleName]))
relationsFrom fileName = do
    src <- readFile fileName
    -- (parseFileWithMode (defaultParseMode { fixities = [] } )
    case parseModuleWithMode (defaultParseMode { fixities = Just [] }) src of
        ParseOk parsed -> return $ Just (relations parsed)
        ParseFailed _ _ -> return Nothing

--mkGraph nodes edges

namesFromModules :: [ModuleName] -> [String]
namesFromModules ((ModuleName name) : t) = [name] ++ namesFromModules t
namesFromModules _ = []

nodesFromRelations :: [(ModuleName, [ModuleName])] -> [String]
nodesFromRelations ((ModuleName name, imports) : t) = [name] ++ namesFromModules imports ++ nodesFromRelations t
nodesFromRelations _ = []

edgesFromRelation :: (ModuleName, [ModuleName]) -> [(String, String)]
edgesFromRelation (ModuleName name, imports) = zip (repeat name) (namesFromModules imports)

edgesFromRelations :: [(ModuleName, [ModuleName])] -> [(String, String)]
edgesFromRelations (h : t) = (edgesFromRelation h) ++ edgesFromRelations t
edgesFromRelations _ = []

dotNodes :: [String] -> [DotNode String]
dotNodes nodes = map (\nodeName -> DotNode nodeName []) nodes

dotEdges :: [(String, String)] -> [DotEdge String]
dotEdges edges = map (\(a, b) -> DotEdge a b []) edges

main :: IO()
main = do
    files <- findHsFiles "."
    maybeRelations <- mapM relationsFrom files
    let found = catMaybes maybeRelations
    let nodes = sort $ nub $ nodesFromRelations $ found
    let edges = sort $ nub $ edgesFromRelations $ found
    let graph = DotGraph False True (Just (Str (pack "h2dot"))) (DotStmts [] [] (dotNodes nodes) (dotEdges edges))
    writeDotFile "hs2dot.dot" graph
