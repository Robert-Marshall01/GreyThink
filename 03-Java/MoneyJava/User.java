/*
VERY IMPORTANT: GitHub Copilot AI generated this code. Please review it carefully before using it, then modified by me.
It may contain errors or security vulnerabilities.

LANGUAGE VERSION: Java 8+

IMPORTANT: This program is made by an indie developer and is not affiliated with any company.
IMPORTANT: Use at your own risk. The developer is not responsible for any damage or loss of data that may occur from using this program.
*/
import java.io.Serializable;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;

public class User implements Serializable {
    private final String email;
    private final String passwordHash;
    private double balance;
    private double income;

    public User(String email, String password, double balance, double income) {
        this.email = email;
        this.passwordHash = hashPassword(password);
        this.balance = balance;
        this.income = income;
    }

    public String getEmail() {
        return email;
    }

    public double getBalance() {
        return balance;
    }

    public double getIncome() {
        return income;
    }

    public void setBalance(double balance) {
        this.balance = balance;
    }

    public void setIncome(double income) {
        this.income = income;
    }

    public boolean checkPassword(String password) {
        return passwordHash.equals(hashPassword(password));
    }

    private String hashPassword(String password) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            byte[] hash = md.digest(password.getBytes());
            return Base64.getEncoder().encodeToString(hash);
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException("SHA-256 not available");
        }
    }
}
