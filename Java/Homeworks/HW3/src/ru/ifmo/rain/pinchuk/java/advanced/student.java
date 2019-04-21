package ru.ifmo.rain.pinchuk.java.advanced;

import info.kgeorgiy.java.advanced.student.Student;
import info.kgeorgiy.java.advanced.student.StudentQuery;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static java.util.List.of;

public class student implements StudentQuery {
    static private Comparator<Student> comparatorId = Comparator.comparing(Student::getId);
    static private Comparator<Student> comparatorFirst = Comparator.comparing(Student::getFirstName);
    static private Comparator<Student> comparatorLastFirstId =
            Comparator.comparing(Student::getLastName)
                    .thenComparing(Student::getFirstName)
                    .thenComparing(Student::getId);

    static private Function<Student, String> mapToFirstName = Student::getFirstName;
    static private Function<Student, String> mapToLastName = Student::getLastName;
    static private Function<Student, String> mapToGroup = Student::getGroup;
    static private Function<Student, String> mapToFullName = s -> s.getFirstName() + " " + s.getLastName();

    private List<String> mapStudents(List<Student> students, Function<Student, String> map) {
        return students.stream().map(map).collect(Collectors.toList());
    }

    @Override
    public List<String> getFirstNames(List<Student> students) {
        return mapStudents(students, mapToFirstName);
    }

    @Override
    public List<String> getLastNames(List<Student> students) {
        return mapStudents(students, mapToLastName);
    }

    @Override
    public List<String> getGroups(List<Student> students) {
        return mapStudents(students, mapToGroup);
    }

    @Override
    public List<String> getFullNames(List<Student> students) {
        return mapStudents(students, mapToFullName);
    }

    @Override
    public Set<String> getDistinctFirstNames(List<Student> students) {
        return  students.stream().map(mapToFirstName).collect(Collectors.toCollection(TreeSet::new));
        //return getFirstNames(students).stream().collect(Collectors.toCollection(TreeSet::new));

    }

    @Override
    public String getMinStudentFirstName(List<Student> students) {
        return students.stream().min(comparatorId).map(Student::getFirstName).orElse("");
    }

    private List<Student> sortStudents(Collection<Student> students, Comparator<Student> comparator) {
        return students.stream().sorted(comparator).collect(Collectors.toList());
    }

    @Override
    public List<Student> sortStudentsById(Collection<Student> students) {
        return sortStudents(students, comparatorId);
    }

    @Override
    public List<Student> sortStudentsByName(Collection<Student> students) {
        return sortStudents(students, comparatorLastFirstId);
    }

    private List<Student> findStudents(Collection<Student> students, Predicate<Student> pred) {
        return students.stream().filter(pred).sorted(comparatorLastFirstId).collect(Collectors.toList());
    }

    @Override
    public List<Student> findStudentsByFirstName(Collection<Student> students, String name) {
        return findStudents(students, s -> s.getFirstName().equals(name));
    }

    @Override
    public List<Student> findStudentsByLastName(Collection<Student> students, String name) {
        return findStudents(students, s -> s.getLastName().equals(name));
    }

    @Override
    public List<Student> findStudentsByGroup(Collection<Student> students, String group) {
        return findStudents(students, s -> s.getGroup().equals(group));
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(Collection<Student> students, String group) {
        return findStudentsByGroup(students, group).stream().collect(Collectors.toMap(mapToLastName, mapToFirstName, (a, b) -> (a.compareTo(b) > 0) ? b : a));
    }
}
