#include <stdio.h>
/*
NOTE: Most of this code is AI-generated, with human oversight and modifications to ensure accuracy and functionality.
NOTE: This program is inspired from a project idea from https://www.w3schools.com/c/c_projects.php
NOTE: Because I am new to C programming, I used AI to help me write this code, so I could learn from it.
NOTE: This code is for educational purposes only, not intended for high-stakes or production use, as it may contain errors or inefficiencies.
It is a simple student gradebook system that allows a teacher to manage and retrieve student grades.
LANGUAGE VERSION: C99 (compatible with GCC)
*/
/*To run the program,
1. Have C/C++ extension installed in your VS Code editor.
2. Install GCC compiler from https://jmeubank.github.io/tdm-gcc/
3. Test the installation by running `gcc --version` in the terminal.
4. Restart VS Code and your terminal after installation.
5. Open the terminal in VS Code.
6. Navigate to the directory where this file is located.
7. Compile and run the program with the following Powershell command:
gcc student_gradebook.c -o student_gradebook
if ($?) { ./student_gradebook }
*/
//The user can search for a student by name and retrieve their grade
#define NAME_LENGTH 10
#define NUMBER_OF_STUDENTS 20
/*You the teacher (or programmer) would input the student names and their grades
at their respective indexes*/
char student_names[NUMBER_OF_STUDENTS][NAME_LENGTH] = {"Alice", "Bob", "Charlie", "David", "Eva", "Frank",
                              "Grace", "Hannah", "Ian", "Jack", "Kathy", "Liam",
                              "Mia", "Nina", "Oscar", "Paul", "Quinn", "Rose",
                              "Sam", "Tina"};
int grades[] = {11, 90, 85, 78, 92, 88, 76, 95, 89, 84, 91, 87,
                93, 82, 77, 94, 86, 80, 79, 83};

// this function grabs data from the student database function for searching
#include <string.h>
void command_list() {
  char command;
  while (1) {
    printf("Enter command (s/a/h/l/q/p/g): ");
    scanf(" %c", &command);
    if (command == 'q') {
      printf("Exiting the program. Goodbye!\n");
      break;
    } else if (command == 's') {
      char name[NAME_LENGTH];
      printf("Enter student name: ");
      scanf("%s", name);
      int found = 0;
      for (int i = 0; i < NUMBER_OF_STUDENTS; i++) {
        if (strcmp(student_names[i], name) == 0) {
          printf("Student: %s, Grade: %d\n", student_names[i], grades[i]);
          found = 1;
          break;
        }
      }
      if (!found) {
        printf("Student not found.\n");
      }
    } else if (command == 'a') {
      int sum = 0;
      for (int i = 0; i < NUMBER_OF_STUDENTS; i++) {
        sum += grades[i];
      }
      printf("Average grade of the class: %.2f\n", sum / (float)NUMBER_OF_STUDENTS);
    } else if (command == 'h') {
      int highest = grades[0];
      for (int i = 1; i < NUMBER_OF_STUDENTS; i++) {
        if (grades[i] > highest) {
          highest = grades[i];
        }
      }
      printf("Highest grade in the class: %d\n", highest);
    } else if (command == 'l') {
      int lowest = grades[0];
      for (int i = 1; i < NUMBER_OF_STUDENTS; i++) {
        if (grades[i] < lowest) {
          lowest = grades[i];
        }
      }
      printf("Lowest grade in the class: %d\n", lowest);
    }
    else if (command == 'p') {
      printf("Student List:\n");
      for (int i = 0; i < NUMBER_OF_STUDENTS; i++) {
        printf("%s\n", student_names[i]);
      }
    } else if (command == 'g') {
      printf("Grades List:\n");
      for (int i = 0; i < NUMBER_OF_STUDENTS; i++) {
        printf("%d\n", grades[i]);
      }
  }else {
      printf("Invalid command. Please try again.\n");
    }
  }
}
int main() {
  printf("================================================\n");
  printf("Welcome to the Student Gradebook System!\n");
  printf("================================================\n");
  printf("Students in the class: %i\n", NUMBER_OF_STUDENTS);
  printf("------------------------------------------------\n");
  printf("Commands:\n");
  printf("s - Search for a student by name\n");
  printf("a - average grade of the class\n");
  printf("h - highest grade in the class\n");
  printf("l - lowest grade in the class\n");
  printf("q - quit the program\n");
  printf("p - print student list\n");
  printf("g - print grades list\n");
  printf("================================================\n");
  command_list();
  return 0;
}
