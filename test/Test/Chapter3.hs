module Test.Chapter3
    ( chapter3
    ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Chapter3


chapter3 :: Spec
chapter3 = describe "Chapter3" $ do
    chapter3normal
    chapter3advanced

chapter3normal :: Spec
chapter3normal = describe "Chapter3Normal" $ do
    describe "Task2: fight" $ do
        it "knight win" $ fight (MkMonster 10 20 30) (MkKnight 1 10 20) `shouldBe` 50
        it "monster win" $ fight (MkMonster 15 20 30) (MkKnight 20 10 20) `shouldBe` -1
        it "draw" $ fight (MkMonster 15 20 30) (MkKnight 25 10 40) `shouldBe` 40
    describe "Task4: City" $ do
        describe "buildCastle" $ do
            it "none" $ buildCastle (MkCity None Library []) "newCastle" `shouldBe` MkCity (WithoutWall "newCastle") Library []
            it "castleWithWall" $ buildCastle (MkCity (WithWall "old") Church []) "newCastle" `shouldBe` MkCity (WithoutWall "newCastle") Church []
            it "castleWithoutWall" $ buildCastle (MkCity (WithoutWall "old") Library [MkHouse 1]) "newCastle" `shouldBe` MkCity (WithoutWall "newCastle") Library [MkHouse 1]
        describe "buildHouse" $ do
            it "invalid number" $ buildHouse mockCity 10 `shouldBe` mockCity
            it "valid number" $ buildHouse mockCity 4 `shouldBe` mockCity { cityHouse = [MkHouse 4] }
        describe "buildWalls" $ do
            it "empty castle" $ buildWalls mockCity `shouldBe` mockCity
            it "not enough people" $ buildWalls (MkCity (WithoutWall "name") Library []) `shouldBe` MkCity (WithoutWall "name") Library []
            it "success" $ buildWalls (MkCity (WithoutWall "name") Library [MkHouse 4, MkHouse 6]) `shouldBe` MkCity (WithWall "name") Library [MkHouse 4, MkHouse 6]
    describe "Task8: Week" $ do
        describe "isWeekend" $ do
            it "Monday" $ isWeekend Monday `shouldBe` False
            it "Tuesday" $ isWeekend Tuesday `shouldBe` False
            it "Wednesday" $ isWeekend Wednesday `shouldBe` False
            it "Thursday" $ isWeekend Thursday `shouldBe` False
            it "Friday" $ isWeekend Friday `shouldBe` False
            it "Saturday" $ isWeekend Saturday `shouldBe` True
            it "Sunday" $ isWeekend Sunday `shouldBe` True
        describe "nextDay" $ do
            it "Monday" $ nextDay Monday `shouldBe` Tuesday
            it "Tuesday" $ nextDay Tuesday `shouldBe` Wednesday
            it "Wednesday" $ nextDay Wednesday `shouldBe` Thursday
            it "Thursday" $ nextDay Thursday `shouldBe` Friday
            it "Friday" $ nextDay Friday `shouldBe` Saturday
            it "Saturday" $ nextDay Saturday `shouldBe` Sunday
            it "Sunday" $ nextDay Sunday `shouldBe` Monday
        describe "daysToParty" $ do
            it "Monday" $ daysToParty Monday `shouldBe` 4
            it "Tuesday" $ daysToParty Tuesday `shouldBe` 3
            it "Wednesday" $ daysToParty Wednesday `shouldBe` 2
            it "Thursday" $ daysToParty Thursday `shouldBe` 1
            it "Friday" $ daysToParty Friday `shouldBe` 7
            it "Saturday" $ daysToParty Saturday `shouldBe` 6
            it "Sunday" $ daysToParty Sunday `shouldBe` 5
    describe "Task9: fight" $ do
            it "run away" $ fight' (Knight' "k" 1 2 3 [KDrinkPotion 1]) (Monster' "m" 1 2 [MRunAway]) `shouldBe` Nothing
            it "knight win" $ fight' (Knight' "k" 50 10 0 [KAttack, KCastSpell 1]) (Monster' "m" 50 10 [MAttack]) 
                `shouldBe` Just (Left (Knight' "k" 10 10 0 [KCastSpell 1, KAttack]))
            it "monster win" $ fight' (Monster' "m" 50 20 [MAttack]) (Knight' "k" 100 10 0 [KDrinkPotion 10, KAttack, KAttack, KCastSpell 10])
                `shouldBe` Just (Left (Monster' "m" 10 20 [MAttack]))

chapter3advanced :: Spec
chapter3advanced = describe "Chapter3Advanced" $ it "" $ True `shouldBe` True

mockCity :: City
mockCity = MkCity None Library []