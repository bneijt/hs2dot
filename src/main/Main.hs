
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

relationsFrom fileName = do
    src <- readFile fileName
    case parseModule src of
        ParseOk mod -> return $ Just (relations mod)
        ParseFailed _ _ -> return Nothing

--mkGraph nodes edges

namesFromModules :: [ModuleName] -> [String]
namesFromModules ((ModuleName name) : t) = [name] ++ namesFromModules t
namesFromModules _ = []

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

main = do
    files <- findHsFiles "."
    maybeRelations <- mapM relationsFrom files
    let relations = catMaybes maybeRelations
    let nodes = nub $ nodesFromRelations $ relations
    let edges = nub $ edgesFromRelations $ relations
    let graph = DotGraph False True (Just (Str (pack "h2dot"))) (DotStmts [] [] (dotNodes nodes) (dotEdges edges))
    writeDotFile "h2dot.dot" graph
    putStrLn $ show edges
