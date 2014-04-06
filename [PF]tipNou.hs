data MyBool = MyTrue | MyFalse

myAnd:: MyBool -> MyBool -> MyBool
myAnd MyTrue MyTrue = MyTrue
myAnd x y = MyFalse 

myOr MyTrue x = MyTrue
myOr x MyTrue = MyTrue
myOr x y = MyFalse

myNot MyTrue = MyFalse
myNot MyFalse = MyTrue