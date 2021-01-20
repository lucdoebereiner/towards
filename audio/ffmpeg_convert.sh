#! /bin/bash

rate=$1

ins=$(find "files/" -name '*.wav')

for i in $ins; do
    basePath=${i%.*}
    baseName=${basePath##*/}
    ffmpeg -y -i "$i" -vn -ar 48000 -ac 1 -b:a $rate "$basePath".mp3
    
    # echo "$basePath".mp3
    # echo $i
done

echo "Conversion from complete!"
