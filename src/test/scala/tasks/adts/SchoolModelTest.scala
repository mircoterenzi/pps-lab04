package tasks.adts

import org.junit.*
import org.junit.Assert.*
import u03.extensionmethods.Sequences.Sequence.*

class SchoolModelTest:

  import tasks.adts.SchoolModel.BasicSchoolModule.*
  val school: School = emptySchool
  val john: Teacher = teacher("John")
  val math: Course = course("Math")
  val italian: Course = course("Italian")
  val school2: School = school.setTeacherToCourse(john, math)
  val school3: School = school2.setTeacherToCourse(john, italian)

  @Test def testTeacher() =
    assertEquals(nil(), school.teachers())
    assertEquals(cons("John", nil()), school2.teachers())
    assertEquals(cons("John", nil()), school3.teachers())
  
  @Test def testCourses() =
    assertEquals(nil(), school.courses())
    assertEquals(cons("Math", nil()), school2.courses())
    assertEquals(cons("Math", cons("Italian", nil())), school3.courses())

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
    assertEquals(nil(), school.coursesOfATeacher(john))
    assertEquals(cons("Math", nil()), school2.coursesOfATeacher(john))
    assertEquals(cons("Math", cons("Italian", nil())), school3.coursesOfATeacher(john))
