#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct  10 09:51:00 2018

@author: manohar
"""

import os
import time as ts
import shutil
import face_recognition

home = str(os.path.dirname(os.path.abspath(__file__))) + "/../../"
input_directory = home + "public/photos/temp/"
report_directory = home + "public/photos/"

while True:
    for in_filename in os.listdir(input_directory):
        delete = 0
        if in_filename.endswith(".jpeg") : 
            File1 = input_directory + in_filename
            for filename in os.listdir(report_directory):
                if filename.endswith(".jpeg") : 
                    File2 = os.path.join(report_directory, filename)
                    imageA = face_recognition.load_image_file(File1)
                    imageB = face_recognition.load_image_file(File2)
                                     
                    image1_encoding = face_recognition.face_encodings(imageA)
                    
                    if len(image1_encoding) != 1:
                        continue
                    
                    image2_encoding = face_recognition.face_encodings(imageB)
                    
                    if len(image2_encoding) != 1:
                        continue
                    
                    image1_check = face_recognition.face_encodings(imageA)[0]
                    image2_check = face_recognition.face_encodings(imageB)[0]
                    
                    result = face_recognition.compare_faces([image1_check], image2_check)
                    
                    print ("File2 :",File2," result: ",result)
        
                    if result[0] == True:
                        os.remove(File1)
                        delete = 1
                        break
            if delete == 0:
                shutil.move(File1,report_directory+in_filename)
    ts.sleep(1)
    print ("Sleeping for 1 sec")