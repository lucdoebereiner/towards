#!/bin/sh

rsync -azPcu root@116.202.8.138:/root/towards/audio/files/ files/
rsync -azPcu files root@116.202.8.138:/root/towards/audio/
