sed "s/\$DAY/$DAY/g" template.hs > "src/Day$DAY.hs"
sed -i '' "s/Day../Day$DAY/g" app/Main.hs
touch "src/day$DAY.txt"
