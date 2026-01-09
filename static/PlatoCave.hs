-- PlatoCave.hs: A conceptual script that models Plato's Allegory of the Cave.
-- This script is a thought experiment on the nature of reality, perception, and enlightenment.

data Reality = Shadow | Form | Good

data Prisoner = Prisoner {
    name :: String,
    perception :: Reality
}

-- A prisoner's perception is limited to the shadows on the wall.
viewShadows :: Prisoner -> String
viewShadows p = "Prisoner " ++ name p ++ " sees only Shadows."

-- The journey out of the cave is a process of enlightenment.
ascendToForms :: Prisoner -> Prisoner
ascendToForms p = p { perception = Form }

-- Beholding the "Good" is the final stage of enlightenment.
beholdTheGood :: Prisoner -> Prisoner
beholdTheGood p = p { perception = Good }

main :: IO ()
main = do
    let socrates = Prisoner { name = "Socrates", perception = Shadow }
    putStrLn $ viewShadows socrates

    let socrates_ascending = ascendToForms socrates
    putStrLn "Socrates is ascending from the cave..."
    putStrLn $ "He now perceives the Forms directly."

    let socrates_enlightened = beholdTheGood socrates_ascending
    putStrLn "Socrates has beheld the Good."
    putStrLn $ "His reality is transformed."
