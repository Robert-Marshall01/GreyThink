#This is a Bash one-liner that analyzes the line count of the GreyThink repo.
git ls-files | grep -vE '\.(txt|md|png|csv|lock)$' | xargs wc -l
