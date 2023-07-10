import Data.These

t :: [These String Int]
t = [This "foo", That 1, These "bar" 2]

-- $> sequence t

main = undefined
