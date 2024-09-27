#!/bin/bash

# Navigate to the repository directory
cd /Users/ethan/Documents/GitHub/ReactionsSystems

# Add all changes
git add .

# Commit with current date and time
git commit -m "Automatic commit on $(date '+%Y-%m-%d %H:%M:%S')"

# Push to the main branch
git push origin main