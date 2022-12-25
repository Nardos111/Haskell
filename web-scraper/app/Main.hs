{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.Scalpel
import Control.Applicative

type Author = String

data Comment
    = TextComment Author String
    | TotalComment Author URL
    deriving (Show, Eq)


main :: IO ()
main = do
    htmlFile <- readFile "../file.txt"
    writeToFile $ scrapeStringLike htmlFile comments

    print $ scrapeStringLike htmlFile description

    print $ scrapeStringLike htmlFile comments
    where
    comments :: Scraper String [Comment]
    comments = chroots ("div" @: [hasClass "style-scope", hasClass  "ytd-comment-renderer", "id" @= "main"]) comment

    description :: Scraper String [String]
    description = 
        chroots ("div" @: [hasClass "style-scope", hasClass "item", hasClass "ytd-watch-metadata", "id" @= "description"]) $ do
            content <- text $ "div" @: ["id" @= "description-inner"]
            return content

    comment :: Scraper String Comment
    comment = textComment <|> totalComment 

    textComment :: Scraper String Comment
    textComment = do
        author      <- text $ "a" @: ["id" @= "author-text"]
        commentText <- text $ "div"  @: ["id" @= "content"]
        return $ TextComment author commentText

    totalComment :: Scraper String Comment
    totalComment = do
        total   <- text $ "span" @: [hasClass "style-scope", hasClass  "yt-formatted-string"]
        authorURL <- attr "src" $ "img"  @: [hasClass "style-scope", hasClass  "yt-img-shadow", "id" @= "img"] 
        return $ TotalComment total authorURL
    
    writeToFile :: Maybe [Comment] -> IO ()
    writeToFile writeComment= do
        writeFile "comments.csv" (show writeComment)

    
    
    


