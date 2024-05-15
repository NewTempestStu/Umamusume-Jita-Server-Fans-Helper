#!/bin/bash

# Step 1: Compile the project
echo "Compiling the project..."
make

# Check if make was successful
if [ $? -ne 0 ]; then
    echo "Compilation failed."
    exit 1
fi

# Step 2: Run the executable
echo "Running the executable..."
./main

# Check if the executable ran successfully
if [ $? -ne 0 ]; then
    echo "Execution failed."
    exit 1
fi

# Step 3: Clean up build files
echo "Cleaning up build files..."
make clean

# Step 4: Delete specific text files
echo "Deleting week1_normalized.txt and week2_normalized.txt..."
rm -f week1_normalized.txt week2_normalized.txt

echo "Workflow completed successfully."
