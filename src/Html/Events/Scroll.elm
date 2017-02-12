module Html.Events.Scroll exposing (ScrollEvent, onScroll, onScrollToBottom, onScrollToTop)

{-| Functions for dealing with [`scroll`] event.

[`scroll`]: https://developer.mozilla.org/en-US/docs/Web/Events/scroll

@docs ScrollEvent, onScroll, onScrollToBottom, onScrollToTop
-}

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as Json


{-| Provides access to target element’s [`clientHeight`], [`scrollHeight`] and [`scrollTop`] properties.

[`clientHeight`]: https://developer.mozilla.org/en-US/docs/Web/API/Element/clientHeight
[`scrollHeight`]: https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollHeight
[`scrollTop`]: https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollTop
-}
type alias ScrollEvent =
    { targetClientHeight : Int
    , targetScrollHeight : Int
    , targetScrollTop : Int
    }


{-| A decoder for `ScrollEvent`.
-}
scrollEvent : Json.Decoder ScrollEvent
scrollEvent =
    Json.map3 ScrollEvent
        (Json.at [ "target", "clientHeight" ] Json.int)
        (Json.at [ "target", "scrollHeight" ] Json.int)
        (Json.at [ "target", "scrollTop" ] Json.int)


{-| Capture [`scroll`] event.

[`scroll`]: https://developer.mozilla.org/en-US/docs/Web/Events/scroll
-}
onScroll : (ScrollEvent -> msg) -> Attribute msg
onScroll tagger =
    on "scroll" (Json.map tagger scrollEvent)


{-| Handle user scrolling to the bottom of an element.
-}
onScrollToBottom : msg -> Attribute msg
onScrollToBottom tagger =
    let
        isBottom evt =
            if evt.targetScrollTop + evt.targetClientHeight == evt.targetScrollHeight then
                Json.succeed tagger
            else
                Json.fail "not bottom"
    in
        on "scroll" (Json.andThen isBottom scrollEvent)


{-| Handle user scrolling to the top of an element.
-}
onScrollToTop : msg -> Attribute msg
onScrollToTop tagger =
    let
        isTop evt =
            if evt.targetScrollTop == 0 then
                Json.succeed tagger
            else
                Json.fail "not top"
    in
        on "scroll" (Json.andThen isTop scrollEvent)
