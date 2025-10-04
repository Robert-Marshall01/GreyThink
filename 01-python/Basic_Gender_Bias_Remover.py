#Some of the code is AI assisted and/or generated
#Simply run the code in Colab
#If that doesn't work, run it in Visual Studio with the Python development workload installed
"""
NOTE: This is a basic implementation and may not cover all edge cases or complex sentences.
It is intended for educational purposes and can be expanded for more comprehensive

NOTE: Because this program was made in Colab, it may not run properly in other IDEs.

NOTE: To run it on Visual Code you must,
1. Install Python extension
2. Change to the directory where this file is located
3. run "python Basic_Gender_Bias_Remover.py"
"""
def assert_equals(computed_output, expected_output):
    if computed_output == expected_output:
        print("passed:", computed_output)
    else:
        print("failed:", computed_output, "!=", expected_output)


def gender_bias_remover(text):
    biased_words = {
        "female": [
            "woman", "girl", "mother", "daughter", "sister", "wife", "aunt", "queen", "princess", "actress",
            "waitress", "hostess", "heroine", "lady", "madam", "ms.", "female", "she", "her", "policewoman", "firewoman"
        ],
        "male": [
            "man", "boy", "father", "son", "brother", "husband", "uncle", "king", "prince", "actor",
            "waiter", "host", "hero", "gentleman", "sir", "mr.", "male", "he", "his", "policeman", "fireman"
        ],
        "neutral": [
            "person", "kid", "parent", "child", "sibling", "spouse", "relative", "monarch", "heir", "performer",
            "server", "host", "protagonist", "guest", "customer", "mx.", "human", "they", "their", "police", "firefighter"
        ]
    }

    # Build a lookup dictionary for quick replacement
    replacement_map = {}
    for f, m, n in zip(biased_words["female"], biased_words["male"], biased_words["neutral"]):
        replacement_map[f] = n
        replacement_map[m] = n

    words = text.split()
    new_words = []

    for i, word in enumerate(words):
        # Strip punctuation (like "." or ",") but remember it
        stripped = word.strip(".,!?")
        punct = word[len(stripped):] if len(stripped) < len(word) else ""

        lower = stripped.lower()
        if lower in replacement_map:
            replacement = replacement_map[lower]
            # Preserve capitalization if it's the first word or originally capitalized
            if stripped[0].isupper():
                replacement = replacement.capitalize()
            new_words.append(replacement + punct)
        else:
            new_words.append(word)

    return " ".join(new_words)


# Example test
"""
text1 = "She the firewoman takes her coat. He the policeman watches his town."
expected = "They the firefighter takes their coat. They the police watches their town."

assert_equals(remove_gender_bias(text1), expected)
"""
# Interactive input
print(gender_bias_remover(input("Insert Text Here: ")))
