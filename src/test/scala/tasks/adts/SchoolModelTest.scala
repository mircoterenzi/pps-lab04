package tasks.adts

import org.junit.*
import org.junit.Assert.*
import u03.Sequences.Sequence.*

class SchoolModelTest:

  import tasks.adts.SchoolModel.BasicSchoolModule.*
  val school = emptySchool
  val john = teacher("John")
  val math = course("Math")
  val italian = course("Italian")
  val school2 = school.setTeacherToCourse(john, math)
  val school3 = school2.setTeacherToCourse(john, italian)

  @Test def testTeacher() =
    assertEquals(Nil(), school.teachers())
    assertEquals(Cons("John", Nil()), school2.teachers())
    assertEquals(Cons("John", Nil()), school3.teachers())
  
  @Test def testCourses() =
    assertEquals(Nil(), school.courses())
    assertEquals(Cons("Math", Nil()), school2.courses())
    assertEquals(Cons("Math", Cons("Italian", Nil())), school3.courses())

  @Test def testHasTeacher() =
    assertFalse(school.hasTeacher("John"))
    assertTrue(school2.hasTeacher("John"))
    assertTrue(school3.hasTeacher("John"))

  @Test def testHasCourse() =
    assertFalse(school.hasCourse("Math"))
    assertTrue(school2.hasCourse("Math"))
    assertFalse(school2.hasCourse("Italian"))
    assertTrue(school3.hasCourse("Math"))
    assertTrue(school3.hasCourse("Italian"))

  @Test def testCoursesOfATeacher() =
    assertEquals(Cons("Math", Cons("Italian", Nil())), school3.coursesOfATeacher(john))
    