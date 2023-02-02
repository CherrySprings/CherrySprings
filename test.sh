#!/bin/bash

function tests {
  bin_files=`eval "find $1 -mindepth 1 -maxdepth 1 -regex \".*\.\(bin\)\""`
  for bin_file in $bin_files; do
    file_name=`basename ${bin_file%.*}`
    printf "[%30s] " $file_name
    log_file=./build/$file_name.log
    ./build/emu --diff=../NEMU/build/riscv64-nemu-interpreter-so -i $bin_file &> $log_file
    if (grep 'HIT GOOD TRAP' $log_file > /dev/null) then
      echo -e "\033[1;32mPASS!\033[0m"
      rm $log_file
    else
      echo -e "\033[1;31mFAIL!\033[0m see $log_file for more information"
    fi
  done
  wait
}

while getopts 'r:t:' OPT; do
  case $OPT in
    r)
      dir="$OPTARG"
      tests ${dir};;
    t)
      test="$OPTARG"
      ./build/emu -i ${test} --dump-wave --wave-path=./build/wave.vcd;;
    ?)
      echo "Error: unknown arguments"
  esac
done
