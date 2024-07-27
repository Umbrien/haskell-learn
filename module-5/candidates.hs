import Data.Map qualified as Map

data Grade = F | D | C | B | A
  deriving (Eq, Ord, Enum, Show, Read)

data Degree = HS | BA | MS | PhD
  deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
  { candidateId :: Int,
    codeReview :: Grade,
    cultureFit :: Grade,
    education :: Degree
  }
  deriving (Show)

viable :: Candidate -> Bool
viable candidate = and tests
  where
    passedCoding = codeReview candidate > B
    passedCultureFit = cultureFit candidate > C
    educationMin = education candidate >= MS
    tests = [passedCoding, passedCultureFit, educationMin]

testCandidate :: Candidate
testCandidate =
  Candidate
    { candidateId = 0,
      codeReview = A,
      cultureFit = B,
      education = MS
    }

readInt :: IO Int
readInt = read <$> getLine

readGrade :: IO Grade
readGrade = read <$> getLine

readDegree :: IO Degree
readDegree = read <$> getLine

readCandidate :: IO Candidate
readCandidate = do
  putStrLn "id:"
  cId <- readInt
  putStrLn "code review grade:"
  codeGrade <- readGrade
  putStrLn "culture fit grade:"
  cultureGrade <- readGrade
  putStrLn "education degree:"
  degree <- readDegree
  return $
    Candidate
      { candidateId = cId,
        codeReview = codeGrade,
        cultureFit = cultureGrade,
        education = degree
      }

passStatement :: Bool -> String
passStatement a = if a then "Passed" else "Failed"

assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  let passed = viable candidate
  return $ passStatement passed

candidate1 :: Candidate
candidate1 =
  Candidate
    { candidateId = 1,
      codeReview = A,
      cultureFit = A,
      education = BA
    }

candidate2 :: Candidate
candidate2 =
  Candidate
    { candidateId = 2,
      codeReview = C,
      cultureFit = A,
      education = PhD
    }

candidate3 :: Candidate
candidate3 =
  Candidate
    { candidateId = 3,
      codeReview = A,
      cultureFit = B,
      education = MS
    }

candidateIDs :: [Candidate] -> [Int]
candidateIDs = fmap candidateId

candidates :: [Candidate]
candidates = [candidate1, candidate2, candidate3]

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList $ zip (candidateIDs candidates) candidates

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
  candidate <- Map.lookup cId candidateDB
  let passed = viable candidate
  return $ passStatement passed

justStatement :: Maybe String -> String
justStatement Nothing = "error: ID not found"
justStatement (Just s) = s

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed = viable candidate
  return $ passStatement passed

assessCandidates :: [Candidate] -> [String]
assessCandidates candidates = map passStatement passed
  where
    passed = map viable candidates

assessCandidate :: (Monad m) => m Candidate -> m String
assessCandidate candidates = do
  candidate <- candidates
  let passed = viable candidate
  return $ passStatement passed