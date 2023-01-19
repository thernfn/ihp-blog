module Web.Controller.Posts where

import Web.Controller.Prelude
import Web.View.Posts.Index
import Web.View.Posts.New
import Web.View.Posts.Edit
import Web.View.Posts.Show
import qualified Text.MMark as MMark

instance Controller PostsController where
    action PostsAction = do
        posts <- query @Post
          |> orderByDesc #createdAt -- post order
          |> fetch -- fetch (query @Post)
        render IndexView { .. } -- shorthand for { posts = posts }

    action NewPostAction = do
        let post = newRecord
        -- newRecord is giving us an empty Post model. It's equivalent to
        -- manually writing Post { id = Default, title = "", body = "" }
        render NewView { .. }

    -- /ShowPost?postId=postId
    -- here we pattern match on the postId field of ShowPostAction
    -- to get the post id of the given request
    action ShowPostAction { postId } = do
        post <- fetch postId
                >>= pure . modify #comments (orderByDesc #createdAt)
                >>= fetchRelated #comments -- also include comments
        render ShowView { .. }

    -- /EditPost?postId=postId
    action EditPostAction { postId } = do
        post <- fetch postId
        render EditView { .. }

    -- pattern match on the postId
    action UpdatePostAction { postId } = do
        post <- fetch postId -- and fetch it
        post
            |> buildPost
            |> ifValid \case
                Left post -> render EditView { .. } -- did not pass validation
                Right post -> do
                    post <- post |> updateRecord -- update the database
                    setSuccessMessage "Post updated"
                    redirectTo EditPostAction { .. }

    -- dealing with POST /CreatePost request
    action CreatePostAction = do
        let post = newRecord @Post
        post
            |> buildPost
            |> ifValid \case
                Left post -> render NewView { .. } 
                Right post -> do
                    post <- post |> createRecord -- save to the database
                    setSuccessMessage "Post created"
                    redirectTo PostsAction

    -- dealing with DELETE /DeletePost?postId=postId request.
    action DeletePostAction { postId } = do
        post <- fetch postId
        deleteRecord post -- delete
        setSuccessMessage "Post deleted"
        redirectTo PostsAction

buildPost post = post
    |> fill @["title", "body"]
    |> validateField #title nonEmpty -- every post has at least a title
    |> validateField #body nonEmpty
    |> validateField #body isMarkdown
    -- fill @["title", "body"] post
    -- read the title and body attributes from the browser request
    -- and fills them into the post record.

isMarkdown :: Text -> ValidatorResult
isMarkdown text =
  case MMark.parse "" text of
    Left _ -> Failure "Please provide valid Markdown"
    Right _ -> Success
