import Control.Applicative (Alternative)
import Control.Monad (guard)
import Data.Bifunctor (bimap)

data Name = Name
  { firstName :: String,
    lastName :: String
  }

instance Show Name where
  show :: Name -> String
  show (Name first last) = mconcat [first, " ", last]

data GradeLevel
  = Freshman
  | Sophmore
  | Junior
  | Senior
  deriving (Eq, Ord, Enum, Show)

data Student = Student
  { studentId :: Int,
    gradeLevel :: GradeLevel,
    studentName :: Name
  }
  deriving (Show)

students :: [Student]
students =
  [ Student 1 Senior (Name "Odrie" "Lord"),
    Student 2 Junior (Name "Leslie" "Silco"),
    Student 3 Freshman (Name "Judit" "Butler"),
    Student 4 Senior (Name "Gi" "Debor"),
    Student 5 Sophmore (Name "Jan" "Bodriar"),
    Student 6 Junior (Name "Julia" "Christleva")
  ]

data Teacher = Teacher {teacherId :: Int, teacherName :: Name} deriving (Show)

teachers :: [Teacher]
teachers =
  [ Teacher 100 (Name "Simona" "de Bovuar"),
    Teacher 200 (Name "Susen" "Thontag")
  ]

data Course = Course
  { courseId :: Int,
    courseTitle :: String,
    teacher :: Int
  }
  deriving (Show)

courses :: [Course]
courses =
  [ Course 101 "French" 100,
    Course 201 "English" 200
  ]

-- _select :: (a -> b) -> [a] -> [b]
_select :: (Monad m) => (a -> b) -> m a -> m b
_select prop vals = do
  val <- vals
  return $ prop val

studentNames :: [String]
-- studentNames = _select (firstName . studentName) students
studentNames = firstName . studentName <$> students

namesGrages :: [(Name, GradeLevel)]
namesGrages = _select (\x -> (studentName x, gradeLevel x)) students

-- _where :: (a -> Bool) -> [a] -> [a]
_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where f vals = do
  val <- vals
  guard $ f val
  return val

startsWith :: Char -> String -> Bool
startsWith c s = c == head s

bNames :: [Name]
bNames = _where (startsWith 'B' . lastName) (_select studentName students)

bStudents :: [Student]
bStudents = _where (startsWith 'B' . lastName . studentName) students

-- _join :: (Eq c) => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a, b)]
_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a, b)
_join data1 data2 accessor1 accessor2 = do
  d1 <- data1
  d2 <- data2
  let dpairs = (d1, d2) -- Cartesian product: all possible pairs
  guard (accessor1 (fst dpairs) == accessor2 (snd dpairs))
  return dpairs

teachersCourses :: [(Teacher, Course)]
teachersCourses = _join teachers courses teacherId teacher

englishTeachers :: [Name]
englishTeachers = selectResult
  where
    joinData = _join teachers courses teacherId teacher
    whereResult = _where ((== "English") . courseTitle . snd) joinData
    selectResult = _select (teacherName . fst) whereResult

_hinq selectQuery joinQuery whereQuery = (selectQuery . whereQuery) joinQuery

_hinqEnglishTeachers :: [Name]
_hinqEnglishTeachers =
  _hinq
    (_select (teacherName . fst))
    (_join teachers courses teacherId teacher)
    (_where ((== "English") . courseTitle . snd))

teachersLastNames :: [String]
teachersLastNames =
  _hinq
    (_select lastName)
    _hinqEnglishTeachers
    (_where (const True))

data HINQ m a b
  = HINQ (m a -> m b) (m a) (m a -> m a)
  | HINQ_ (m a -> m b) (m a)

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause (_where (const True))

englishTeachersHINQ :: HINQ [] (Teacher, Course) Name
englishTeachersHINQ =
  HINQ
    (_select (teacherName . fst))
    (_join teachers courses teacherId teacher)
    (_where ((== "English") . courseTitle . snd))

hinqEnglishTeachers :: [Name]
hinqEnglishTeachers = runHINQ englishTeachersHINQ

teacherNamesHINQ :: HINQ [] Teacher Name
teacherNamesHINQ = HINQ_ (_select teacherName) teachers

-- Maybe

possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

maybeQueryFrenchTeachers :: HINQ Maybe (Teacher, Course) Name
maybeQueryFrenchTeachers =
  HINQ
    (_select (teacherName . fst))
    (_join possibleTeacher possibleCourse teacherId teacher)
    (_where ((== "French") . courseTitle . snd))

data Enrollment = Enrollment
  { student :: Int,
    course :: Int
  }
  deriving (Show)

enrollments :: [Enrollment]
enrollments =
  [ Enrollment 1 101,
    Enrollment 2 101,
    Enrollment 2 201,
    Enrollment 3 101,
    Enrollment 4 201,
    Enrollment 4 101,
    Enrollment 5 101,
    Enrollment 6 201
  ]

studentEnrollmentsQ :: HINQ [] (Student, Enrollment) (Name, Int)
studentEnrollmentsQ =
  HINQ_
    (_select (bimap studentName course))
    (_join students enrollments studentId student)

studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQ

englishStudentsQ :: HINQ [] ((Name, Int), Course) Name
englishStudentsQ =
  HINQ
    (_select (fst . fst))
    (_join studentEnrollments courses snd courseId)
    (_where ((== "English") . courseTitle . snd))

englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQ

getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
  where
    courseQuery =
      HINQ
        (_select (fst . fst))
        (_join studentEnrollments courses snd courseId)
        (_where ((== courseName) . courseTitle . snd))
