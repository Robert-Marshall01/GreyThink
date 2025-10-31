![LOC](https://tokei.rs/b1/github/Robert-Marshall01/GreyThink)
<h1>Lines of Code Above</h1>
<p>The above number may be up to 20% inflated due to text files.</p>
<h1>Who Am I, and My Specialty 🧑‍💻</h1>

---

I am Robert-Marshall01, and my main goal is to change the world through my code. When I was 12, my passion for computing lit up after being fascinated with how powerful computers were at solving mental grunt work like memorizing random historcal events and names. For more information, I am currently a freshman at Utah Tech University, and my major is in Computer Science, BS (Bachelor of Science). My passion career is to be a programmer (more preferably one that specializes in AI if possible).

<br>

<h2>The Purpose of the Project ⭐</h2>

---

The purpose of the project is to offer various programs coded in various languages. The end goal is to give more power to the programmer that needs a certain script that is written in the same language as their program is. My main philosphy behind my reasoning is to give programmers a variety of options to choose from when coding their projects. 

<br>

<h2>Key Features and Skills Demonstrated 🔑🛠️</h2>

---

The key feature is having a variety of programs coded in different languages. The main skill being demonstrated in the repository is the ability to work with a variety of coding languages. It is helpful to tech companies to be fluent in multiple languages, as different languages are best suited for different roles; for example, Python is best suited for AI and machine learning, while JavaScript shines best in web development. As it goes without saying, "Use the right tool for the job" - Terrance Clancey.

<br>

<h2>How to Install and Run the Code ⬇️▶️</h2>

---

When installing my code, you can either copy-paste it into an IDE that supports the language the code is written, or downloading it onto an IDE. When you do use the code either-way, you must have the legal information that is found under the name of "LICENSE" that carries the necessary legal information. A final note, if you are in K-12 school or college, regardless of what your teacher tells you, you do <b>not</b> need to cite it in MLA, APA, Chicago, or some other academic citation style. As mentioned earlier, copying what is on the "LICENSE" file and pasting it on the bottom is sufficient.

<br>

<h2>Screenshots or Code Examples 📷</h2>

---

```
#Some of the code is AI assisted and/or generated
#Simply run the code in Colab
#If that doesn't work, run it in Visual Studio with the Python development workload installed
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
```

Signed: Robert-Marshall01@10/3/25
