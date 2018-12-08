#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov  8 10:55:20 2018

@author: manohar
"""

import os
import face_recognition
import sys

input_file = sys.argv[1]
matched_files = []
home = str(os.path.dirname(os.path.abspath(__file__))) + "/../../"
directory = home + "public/reported/"
File1 = face_recognition.load_image_file(input_file)

image1_encoding = face_recognition.face_encodings(File1)
        
if len(image1_encoding) != 1:
    print ("Input File error")
        
image1_check = face_recognition.face_encodings(File1)[0]

for filename in os.listdir(directory):
    if filename.endswith(".jpeg") : 
        File2 = os.path.join(directory, filename)
        rep_file = face_recognition.load_image_file(File2)
                                     
        image2_encoding = face_recognition.face_encodings(rep_file)
        
        if len(image2_encoding) != 1:
            continue
        
        image2_check = face_recognition.face_encodings(rep_file)[0]
        
        result = face_recognition.compare_faces([image2_check], image1_check,tolerance=0.6)
        
        if result[0] == True:
            matched_files.append(filename)
            continue
        else:
            continue   
    else:
        continue
    
print (matched_files)
sys.stdout.flush()