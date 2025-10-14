/*
    VERY IMPORTANT: GitHub Copilot AI generated this code. Please review it carefully before using it.
    It may contain errors or security vulnerabilities.
    
    IMPORTANT: This program is made by an indie developer and is not affiliated with any company.
    IMPORTANT: Use at your own risk. The developer is not responsible for any damage or loss of data that may occur from using this program.
    
    EspressoCashGUI - Java Swing GUI Version
    ----------------------------------------
    This is the graphical version of the Espresso Cash money manager.
    Features:
      - Deposit and withdraw money
      - Set and view income
      - View current balance

    Instructions to Run:
      1. Make sure you have Java JDK installed (https://www.oracle.com/java/technologies/javase-downloads.html)
      2. Open a terminal and navigate to the folder containing this file.
      3. Compile the program:
         javac EspressoCashGUI.java
      4. Run the program:
         java EspressoCashGUI

    Notes:
      - This version uses Java Swing for the GUI.
      - You can expand this program to include budget categories and more features.
      - No data is saved between runs (session is not persistent).
      - For questions or issues, review the code and Java Swing documentation.
*/
import javax.swing.*;
import java.awt.*;
import java.io.IOException;

public class EspressoCashGUI extends JFrame {
    private final User user;
    private final JLabel balanceLabel;
    private final JLabel incomeLabel;
    private JTextField depositField;
    private JTextField withdrawField;
    private final JButton depositButton;
    private final JButton withdrawButton;
    private JTextField incomeField;
    private final JButton setIncomeButton;
    private final JButton deleteAccountButton;

    public EspressoCashGUI(User user) {
        this.user = user;
        setTitle("Espresso Cash - Money Manager");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setSize(400, 300);
        setLocationRelativeTo(null);
        setLayout(new GridLayout(0, 1));

        balanceLabel = new JLabel("Balance: $" + user.getBalance());
        incomeLabel = new JLabel("Income: $" + user.getIncome());
        add(balanceLabel);
        add(incomeLabel);

        depositField = new JTextField();
        depositButton = new JButton("Deposit");
        add(new JLabel("Deposit Amount:"));
        add(depositField);
        add(depositButton);

        withdrawField = new JTextField();
        withdrawButton = new JButton("Withdraw");
        add(new JLabel("Withdraw Amount:"));
        add(withdrawField);
        add(withdrawButton);

        incomeField = new JTextField();
        setIncomeButton = new JButton("Set Income");
        add(new JLabel("Set Income:"));
        add(incomeField);
        add(setIncomeButton);

        // Add Delete Account button
        deleteAccountButton = new JButton("Delete Account");
        add(deleteAccountButton);
        deleteAccountButton.addActionListener(e -> {
            int confirm = JOptionPane.showConfirmDialog(this, "Are you sure you want to delete your account? This cannot be undone.", "Delete Account", JOptionPane.YES_NO_OPTION);
            if (confirm == JOptionPane.YES_OPTION) {
                deleteUserAccount(user.getEmail());
                JOptionPane.showMessageDialog(this, "Account deleted. Goodbye!");
                System.exit(0);
            }
        });

        depositButton.addActionListener(e -> {
            try {
                double amount = Double.parseDouble(depositField.getText());
                if (amount > 0) {
                    user.setBalance(user.getBalance() + amount);
                    updateLabels();
                } else {
                    JOptionPane.showMessageDialog(this, "Deposit must be positive.");
                }
            } catch (NumberFormatException ex) {
                JOptionPane.showMessageDialog(this, "Invalid deposit amount.");
            }
        });

        withdrawButton.addActionListener(e -> {
            try {
                double amount = Double.parseDouble(withdrawField.getText());
                if (amount > 0 && amount <= user.getBalance()) {
                    user.setBalance(user.getBalance() - amount);
                    updateLabels();
                } else if (amount > user.getBalance()) {
                    JOptionPane.showMessageDialog(this, "Insufficient funds.");
                } else {
                    JOptionPane.showMessageDialog(this, "Withdrawal must be positive.");
                }
            } catch (NumberFormatException ex) {
                JOptionPane.showMessageDialog(this, "Invalid withdrawal amount.");
            }
        });

        setIncomeButton.addActionListener(e -> {
            try {
                double newIncome = Double.parseDouble(incomeField.getText());
                if (newIncome >= 0) {
                    user.setIncome(newIncome);
                    updateLabels();
                } else {
                    JOptionPane.showMessageDialog(this, "Income must be non-negative.");
                }
            } catch (NumberFormatException ex) {
                JOptionPane.showMessageDialog(this, "Invalid income amount.");
            }
        });
    }

    private void updateLabels() {
        balanceLabel.setText("Balance: $" + String.format("%.2f", user.getBalance()));
        incomeLabel.setText("Income: $" + String.format("%.2f", user.getIncome()));
    }

    // Remove user from users.dat file
    private void deleteUserAccount(String email) {
        try {
            java.io.File file = new java.io.File("users.dat");
            java.util.HashMap<String, User> users;
            if (file.exists()) {
                try (java.io.ObjectInputStream ois = new java.io.ObjectInputStream(new java.io.FileInputStream(file))) {
                    users = (java.util.HashMap<String, User>) ois.readObject();
                } catch (Exception e) {
                    users = new java.util.HashMap<>();
                }
                users.remove(email);
                try (java.io.ObjectOutputStream oos = new java.io.ObjectOutputStream(new java.io.FileOutputStream(file))) {
                    oos.writeObject(users);
                }
            }
        } catch (IOException e) {
            JOptionPane.showMessageDialog(this, "Error deleting account.");
        }
    }

    // Main method is not used when launched from LoginSignupGUI
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            new LoginSignupGUI().setVisible(true);
        });
    }
}
