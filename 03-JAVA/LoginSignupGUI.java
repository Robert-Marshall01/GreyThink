/*
VERY IMPORTANT: GitHub Copilot AI generated this code. Please review it carefully before using it.
It may contain errors or security vulnerabilities.
    
IMPORTANT: This program is made by an indie developer and is not affiliated with any company.
IMPORTANT: Use at your own risk. The developer is not responsible for any damage or loss of data that may occur from using this program.

TO RUN THIS FILE:
1. Make sure you have Java JDK installed (https://www.oracle.com/java/techn
ologies/javase-downloads.html)
2. Open a terminal and navigate to the folder containing this file.
3. Compile the program:
   javac LoginSignupGUI.java
4. Run the program:
   java LoginSignupGUI
*/
import javax.swing.*;
import java.awt.*;
import java.io.*;
import java.util.HashMap;

public class LoginSignupGUI extends JFrame {
    private final JTextField emailField;
    private final JPasswordField passwordField;
    private final JButton loginButton;
    private final JButton signupButton;
    private HashMap<String, User> users;
    private static final String USER_FILE = "users.dat";

    public LoginSignupGUI() {
        setTitle("Espresso Cash - Login or Signup");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setSize(350, 200);
        setLocationRelativeTo(null);
        setLayout(new GridLayout(0, 1));

        emailField = new JTextField();
        passwordField = new JPasswordField();
        loginButton = new JButton("Login");
        signupButton = new JButton("Signup");

        add(new JLabel("Email:"));
        add(emailField);
        add(new JLabel("Password:"));
        add(passwordField);
        add(loginButton);
        add(signupButton);

        loadUsers();

        loginButton.addActionListener(e -> login());
        signupButton.addActionListener(e -> signup());
    }

    private void loadUsers() {
        try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(USER_FILE))) {
            users = (HashMap<String, User>) ois.readObject();
        } catch (Exception e) {
            users = new HashMap<>();
        }
    }

    private void saveUsers() {
        try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(USER_FILE))) {
            oos.writeObject(users);
        } catch (IOException e) {
            JOptionPane.showMessageDialog(this, "Error saving users.");
        }
    }

    private void login() {
        String email = emailField.getText().trim();
        String password = new String(passwordField.getPassword());
        User user = users.get(email);
        if (user != null && user.checkPassword(password)) {
            JOptionPane.showMessageDialog(this, "Login successful!");
            openMainApp(user);
        } else {
            JOptionPane.showMessageDialog(this, "Invalid email or password.");
        }
    }

    private void signup() {
        String email = emailField.getText().trim();
        String password = new String(passwordField.getPassword());
        if (users.containsKey(email)) {
            JOptionPane.showMessageDialog(this, "User already exists.");
            return;
        }
        if (email.isEmpty() || password.isEmpty()) {
            JOptionPane.showMessageDialog(this, "Email and password required.");
            return;
        }
        User newUser = new User(email, password, 10000.0, 3000.0);
        users.put(email, newUser);
        saveUsers();
        JOptionPane.showMessageDialog(this, "Signup successful! You can now login.");
    }

    private void openMainApp(User user) {
        this.dispose();
        EspressoCashGUI mainApp = new EspressoCashGUI(user);
        mainApp.setVisible(true);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            new LoginSignupGUI().setVisible(true);
        });
    }
}
