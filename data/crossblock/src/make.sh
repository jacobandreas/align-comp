out_file='../exe/learner.py'

# create learner module file by concatenating all components ...
cat game.py > $out_file
cat learner_main.py >> $out_file
cat learner_sample.py >> $out_file
cat learner_update.py >> $out_file
cat learner_feature.py >> $out_file
cat learner_file.py >> $out_file
cat learner_init.py >> $out_file

cat config.py >> $out_file
cat interact.py >> $out_file
cat model.py >> $out_file

# compile to & compile C module ...
# rm -f ../exe/learner.so
#cp create_c_module.py ../exe/build
# cd ../exe/build
#python2.5 create_c_module.py build_ext --inplace
#mv learner.so ../

# copy over rest of the source ...
# cd ../../src

# cp $out_file ../exe/learner.py
cp run.py ../exe
# cp cache.py ../exe
# cp config.py ../exe
