module Main
  ( main
  )
  where

import Prelude

import Data.Array (any, filter, length, modifyAt, (:))
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Specular.Dom.Browser (Event)
import Specular.Dom.Element (attr, bindValueOnInput, el, el_, on, onClick_, text)
import Specular.Dom.Widget (Widget, emptyWidget, runMainWidgetInBody)
import Specular.FRP (dynamic_)
import Specular.FRP.List (dynamicListWithIndex_)
import Specular.Ref (Ref)
import Specular.Ref as Ref
import Storage.Storage as Storage
import Unsafe.Coerce (unsafeCoerce)

foreign import data Node :: Type


type Todo = { description :: String, completed :: Boolean } 

data TodoError = Validation | None

main :: Effect Unit
main = do
  -- | Will append widget to the body
  runMainWidgetInBody do
    appState :: Ref (Array Todo) <- Ref.new ([])

    -- liftEffect $ syncState appState

    addWidget appState
    displayWidget appState
    cleanupWidget appState
    counterWidget appState
    completeWidget appState

  where
    submit :: String -> Ref (Array Todo) -> Effect TodoError
    submit "" _ = pure Validation
    submit description todoState = do
      Ref.modify todoState ({ description, completed: false } : _)
      pure None

    submitWrapper :: Ref (Array Todo) -> Ref String -> Ref TodoError -> String -> Effect Unit 
    submitWrapper todoState formState errorState formValue = do
        value <- submit (trim formValue) todoState 

        Ref.modify formState (const "")
        Ref.modify errorState (const value)

    unsafeEventToKeycode :: Event -> Int
    unsafeEventToKeycode = unsafeCoerce >>> _.keyCode
    
    unsafeEventToValue :: Event -> String
    unsafeEventToValue = unsafeCoerce >>> _.target >>> _.value

    addWidget :: Ref (Array Todo) -> Widget Unit
    addWidget todoState = do
      formState :: Ref String <- Ref.new ""
      errorState :: Ref TodoError <- Ref.new None  

      let dynamicForm = Ref.value formState
      let dynamicError = Ref.value errorState

      let onSubmit = submitWrapper todoState formState errorState

      el "input" [attr "autofocus" "true", bindValueOnInput formState, on "keydown" (\e -> do
        log $ unsafeEventToValue e

        if unsafeEventToKeycode e == 13 
          then onSubmit $ unsafeEventToValue e
          else pure unit      
      )] emptyWidget

      dynamic_ $ dynamicForm <#> (\formText -> el "button" [onClick_ (onSubmit formText)] do
        text "Add")

      dynamic_ $ dynamicError <#> errorWidget
    
    errorWidget :: TodoError -> Widget Unit
    errorWidget None = emptyWidget
    errorWidget Validation = text "Validation error"

    displayWidget :: Ref (Array Todo) -> Widget Unit
    displayWidget todoState = do
      dynamicListWithIndex_ (Ref.value todoState) \index todoItem -> do
        let completeCb = Ref.modify todoState (modifyAt' index _ {completed = true})
        dynamic_ $ todoItem <#> \{completed, description} -> elementWidget completed description completeCb

    elementWidget :: Boolean -> String -> Effect Unit -> Widget Unit
    elementWidget true description _ = do 
      el_ "div" do
        el "span" [ attr "style" "text-decoration: line-through" ] do
          text description

    elementWidget false description completeCb = do 
      el_ "div" do
        el_ "span" do
          text description
        el "button" [onClick_ completeCb] do
          text "Complete"

    cleanupWidget :: Ref (Array Todo) -> Widget Unit
    cleanupWidget todoState = do
      let dynamicTodo = Ref.value todoState

      let cleanupCb = Ref.modify todoState $ filter (not _.completed)

      dynamic_ $ dynamicTodo <#> \todos -> if any (_.completed) todos then 
        el "button" [ onClick_ cleanupCb ] $ text "Cleanup"
      else emptyWidget
    
    completeWidget :: Ref (Array Todo) -> Widget Unit
    completeWidget todoState = do
      let dynamicTodo = Ref.value todoState
      let completeCb = Ref.modify todoState $ map (_ {completed = true})

      dynamic_ $ dynamicTodo <#> \todos -> if any (not _.completed) todos then 
        el "button" [ onClick_ completeCb ] $ text "Complete All"
      else emptyWidget

    counterWidget :: Ref (Array Todo) -> Widget Unit
    counterWidget todoState = do
      let dynamicTodo = Ref.value todoState

      dynamic_ $ dynamicTodo <#> filter (not _.completed) >>> length >>> show >>> flip (<>) " tasks left" >>> text

    modifyAt' :: forall a. Int -> (a -> a) -> (Array a) -> (Array a) 
    modifyAt' index fn array = 
      case modifyAt index fn array of
        Just modified -> modified
        Nothing       -> array