
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import System.FilePath
import System.Directory
import Control.Monad
import Data.Maybe
import Data.List
import Control.Applicative
src = "\nimport Distribution.Simple \nmain = putStrLn \"asldkjf\""


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

main = do
    files <- findHsFiles "."
    maybeRelations <- mapM relationsFrom files
    let relations = catMaybes maybeRelations
    let nodes = nub $ nodesFromRelations $ relations
    let edges = nub $ edgesFromRelations $ relations
--    graph <- mkGraph nodes edges
--    writeDotFile "state.dot" graph
    putStrLn $ show edges
