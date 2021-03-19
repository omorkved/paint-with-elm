module Deque exposing
  ( Deque, empty, isEmpty
  , addFront, removeFront, peekFront
  , addBack, removeBack, peekBack
  -- new functions for project:
  , mapToDeque, map2ToDeque, indexedMap, squishToList
  , length, repeat
  )

type Deque a = D { front : List a, back : List a }

empty : Deque a
empty = D {front = [], back = []}

isEmpty : Deque a -> Bool
isEmpty q = q == empty

--------------------------------------------------------------------------------
-- FILL IN THE DEFINITIONS BELOW

{-- checking whether either list is empty and, if so, 
  splitting the other in half and reversing one of the halves.

  Overall: Worst case O(n), since we may call a case that calls List.reverse
  --}
check : List a -> List a -> Deque a
check f b =
  case (List.isEmpty f, List.isEmpty b) of
    (True, True) -> -- This case is constant
      D {front = [], back = []}
    (False, False) -> -- This case is constant
      D {front = f, back = b}
    (True, False) -> -- This case is linear, since we call List.reverse
      let
        lenb = (List.length b) // 2
      in
      D {front = List.reverse (List.drop lenb b)
        , back = List.take lenb b
        }
    (False, True) -> -- This case is linear, since we call List.reverse
      let 
        lenf = (List.length f) // 2
      in
      D {front = List.take lenf f
        , back = List.reverse (List.drop lenf f)
        }


addFront : a -> Deque a -> Deque a
addFront x (D {front, back}) =
  check (x::front) back

addBack : a -> Deque a -> Deque a
addBack x (D {front, back}) =
  check front (x::back)

peekFront : Deque a -> Maybe a
peekFront (D {front, back}) =
  case front of
    x::_ ->
      Just x
    _ ->
      case back of
        --assuming the invariant holds, there is one elt. max in back. So:
        y::_ ->
          Just y
        _ ->
          Nothing

peekBack : Deque a -> Maybe a
peekBack (D {front, back}) =
  case back of
    x::_ ->
      Just x
    _ ->
      case front of
        y::_ ->
          Just y
        _ ->
          Nothing

{-- Assume the invariant is guaranteed to hold for the argument upon
    which the function is INITIALLY called. ie no need to check inputs --}
removeFront : Deque a -> Maybe (Deque a)
removeFront (D {front, back}) =
  case front of
    [] ->
      case back of
        [] ->
          Nothing
        _ ->
          --assuming the invariant holds, there is one elt. max in back. So:
          Just (D { front = [], back = [] })
    x :: [] ->
      Just (check [] back)
    x::xs ->
      Just (check xs back)


{-- Assume the invariant is guaranteed to hold for the argument upon
    which the function is initially called. --}
removeBack : Deque a -> Maybe (Deque a)
removeBack (D {front, back}) =
    case back of
    [] ->
      case front of
        [] ->
          Nothing
        _ ->
          --assuming the invariant holds for the initial argument, there is one elt. max in front. So:
          Just (D { front = [], back = [] })
    x :: [] ->
      Just (check front [])
    x::xs ->
      Just (check front xs)


mapToDeque : (a -> result) -> Deque a -> Deque result
mapToDeque fcn (D {front, back}) =
  D { front = List.map fcn front
    , back = List.map fcn back }


map2ToDeque : (a -> b -> result) -> Deque a -> Deque b -> Deque result
map2ToDeque fcn dOne dTwo =
  let
    (fOne, bOne) = 
      case dOne of
        D{front, back} -> (front, back)

    (fTwo, bTwo) = 
      case dTwo of
        D{front, back} -> (front, back)
  in
  D {
    front = List.map2 fcn fOne fTwo
    , back = List.map2 fcn bOne bTwo
  }
  

indexedMap : Deque a -> Deque (Int, a)
indexedMap (D {front, back}) =
  let
    addToIndex (i, elt) = ((List.length back) + (i - 1), elt)
  in
  D {
    front = List.reverse (List.map addToIndex (List.indexedMap Tuple.pair (List.reverse front)))
    --todo fix back so that it's faster...
    , back = (List.indexedMap Tuple.pair back)
  }


-- deal with calls to other libraries that require a list:
squishToList : Deque a -> List a
squishToList (D {front, back}) =
  List.append front (List.reverse back)


repeat : Int -> a -> Deque a
repeat i elt =
  let
    repeat_ : Int -> Deque a -> Deque a
    repeat_ i_ currD =
      if i_ <= 0 then currD
      else repeat_ (i_ - 1) (addFront elt currD)
  in
    repeat_ i empty

length : Deque a -> Int
length (D{front, back}) =
  (List.length front) + (List.length back)