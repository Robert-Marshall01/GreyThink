{
  "nbformat": 4,
  "nbformat_minor": 0,
  "metadata": {
    "colab": {
      "provenance": []
    },
    "kernelspec": {
      "name": "python3",
      "display_name": "Python 3"
    },
    "language_info": {
      "name": "python"
    }
  },
  "cells": [
    {
      "cell_type": "markdown",
      "source": [
        "Basic Gender Bias Remover\n",
        "\n",
        "\n",
        "---\n",
        "\n"
      ],
      "metadata": {
        "id": "S13dqaGq24kg"
      }
    },
    {
      "cell_type": "code",
      "execution_count": null,
      "metadata": {
        "colab": {
          "base_uri": "https://localhost:8080/"
        },
        "id": "aAyZRoxPQnQK",
        "outputId": "102e3738-2567-46fc-e6ad-053086af5d6e"
      },
      "outputs": [
        {
          "output_type": "stream",
          "name": "stdout",
          "text": [
            "Insert Text Here: she, he; woman. Man\n",
            "they, he; person. Person\n"
          ]
        }
      ],
      "source": [
        "def assert_equals(computed_output, expected_output):\n",
        "    if computed_output == expected_output:\n",
        "        print(\"passed:\", computed_output)\n",
        "    else:\n",
        "        print(\"failed:\", computed_output, \"!=\", expected_output)\n",
        "\n",
        "\n",
        "def gender_bias_remover(text):\n",
        "    biased_words = {\n",
        "        \"female\": [\n",
        "            \"woman\", \"girl\", \"mother\", \"daughter\", \"sister\", \"wife\", \"aunt\", \"queen\", \"princess\", \"actress\",\n",
        "            \"waitress\", \"hostess\", \"heroine\", \"lady\", \"madam\", \"ms.\", \"female\", \"she\", \"her\", \"policewoman\", \"firewoman\"\n",
        "        ],\n",
        "        \"male\": [\n",
        "            \"man\", \"boy\", \"father\", \"son\", \"brother\", \"husband\", \"uncle\", \"king\", \"prince\", \"actor\",\n",
        "            \"waiter\", \"host\", \"hero\", \"gentleman\", \"sir\", \"mr.\", \"male\", \"he\", \"his\", \"policeman\", \"fireman\"\n",
        "        ],\n",
        "        \"neutral\": [\n",
        "            \"person\", \"kid\", \"parent\", \"child\", \"sibling\", \"spouse\", \"relative\", \"monarch\", \"heir\", \"performer\",\n",
        "            \"server\", \"host\", \"protagonist\", \"guest\", \"customer\", \"mx.\", \"human\", \"they\", \"their\", \"police\", \"firefighter\"\n",
        "        ]\n",
        "    }\n",
        "\n",
        "    # Build a lookup dictionary for quick replacement\n",
        "    replacement_map = {}\n",
        "    for f, m, n in zip(biased_words[\"female\"], biased_words[\"male\"], biased_words[\"neutral\"]):\n",
        "        replacement_map[f] = n\n",
        "        replacement_map[m] = n\n",
        "\n",
        "    words = text.split()\n",
        "    new_words = []\n",
        "\n",
        "    for i, word in enumerate(words):\n",
        "        # Strip punctuation (like \".\" or \",\") but remember it\n",
        "        stripped = word.strip(\".,!?\")\n",
        "        punct = word[len(stripped):] if len(stripped) < len(word) else \"\"\n",
        "\n",
        "        lower = stripped.lower()\n",
        "        if lower in replacement_map:\n",
        "            replacement = replacement_map[lower]\n",
        "            # Preserve capitalization if it's the first word or originally capitalized\n",
        "            if stripped[0].isupper():\n",
        "                replacement = replacement.capitalize()\n",
        "            new_words.append(replacement + punct)\n",
        "        else:\n",
        "            new_words.append(word)\n",
        "\n",
        "    return \" \".join(new_words)\n",
        "\n",
        "\n",
        "# Example test\n",
        "\"\"\"\n",
        "text1 = \"She the firewoman takes her coat. He the policeman watches his town.\"\n",
        "expected = \"They the firefighter takes their coat. They the police watches their town.\"\n",
        "\n",
        "assert_equals(remove_gender_bias(text1), expected)\n",
        "\"\"\"\n",
        "# Interactive input\n",
        "print(gender_bias_remover(input(\"Insert Text Here: \")))\n"
      ]
    }
  ]
}
