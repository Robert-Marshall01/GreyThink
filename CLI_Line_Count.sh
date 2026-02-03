#This is a Bash one-liner that analyzes the line count of the GreyThink repo.
# Use NUL-separated paths so filenames with spaces are handled correctly
git ls-files -z | grep -z -vE '\.(txt|md|png|csv|lock|json|yml|yaml|xml)$' \
  | xargs -0 cat | wc -l

