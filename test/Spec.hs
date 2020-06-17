module Spec where
import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Step" $ do
    it "Return KeepAsking" $
      let 
          initialState = game "Juego" 
          finalState = Game "Juego" "_____" 3 1 "a"
      in step initialState 'a' `shouldBe` (finalState , KeepAsking)
    it "Return KeepAsking when input is the same key" $
      let 
          initialState = Game "Juego" "_____" 3 1 "a"
          finalState = Game "Juego" "_____" 3 1 "a"
      in step initialState 'a' `shouldBe` (finalState , LetterAlreadyExist)
    it "Return with a try letter" $
      let 
          initialState = game "Juego" 
          (finalState , _) = step initialState 'a' 
      in tryLetters finalState `shouldBe` "a"
    it "Return GameWin" $
      let 
          initialState = Game "Juego" "Jueg" 3 1 "a"
          (_ , result) = step initialState 'o' 
      in result `shouldBe` GameWin
    it "Return GameLoose" $
      let 
          initialState = Game "Juego" "Jueg_" 3 3 "a"
          (_ , result) = step initialState 't' 
      in result `shouldBe` GameLoose
  describe "ReplaceLetterWithUnderscore" $ do
    it "Replace the char with _" $
      (replaceLetterWithUnderscore 'a' 'a' '_') `shouldBe` "a"
    it "No replace the char with _ when already has letter" $
      (replaceLetterWithUnderscore 'a' 'a' 'b') `shouldBe` "b"
