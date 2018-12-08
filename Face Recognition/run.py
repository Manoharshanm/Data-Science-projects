#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Sep 18 20:47:20 2018

@author: Hashrebels
"""

# imports
import sys,numpy as np
import face_recognition
import cv2
import time
import glob,os
from pathlib import Path
home = str(os.path.dirname(os.path.abspath(__file__))) + "/../../"
file_names = glob.glob(home + "/public/known_people/*.jp*g")
#end

#Read data from stdin
def read_in():
    lines = sys.stdin.readline()
    # Since our input would only be having one line, parse our JSON data from that
    return lines

# Function to check if the person is authorised based on certain parameters
def authorised(name):
    # Assuming if person is not in Database then it is Un-authorised
    return not "Unknown" in name

# Function to send email
def send_email(filename,txt):
    import smtplib
    from email.mime.multipart import MIMEMultipart
    from email.mime.text import MIMEText
    from email.mime.base import MIMEBase
    from email import encoders

    toaddr = "hacksterz18@gmail.com"
    fromaddr = "hacksterz18@gmail.com"

    msg = MIMEMultipart()

    msg['From'] = fromaddr
    msg['To'] = toaddr
    msg['Subject'] = "Unknown person identified"

    msg.attach(MIMEText(txt, 'plain'))

    attachment = open(filename, "rb")

    part = MIMEBase('application', 'octet-stream')
    part.set_payload((attachment).read())
    encoders.encode_base64(part)
    part.add_header('Content-Disposition', "attachment; filename= %s" % filename)

    msg.attach(part)

    server = smtplib.SMTP('smtp.gmail.com', 587)
    server.starttls()
    server.login(fromaddr, "Login@123")
    text = msg.as_string()
    server.sendmail(fromaddr, toaddr, text)
    server.quit()

def main():
    # GETTING KNOWN ENCODINGS AND NAMES
    home = str(os.path.dirname(os.path.abspath(__file__))) + "/../../"
    known_encodings_file_path = home + "/data/known_encodings_file.csv"
    people_file_path = home + "/data/people_file.csv"
    # For storing the encoding of a face
    known_encodings_file = Path(known_encodings_file_path)
    if known_encodings_file.is_file():
        known_encodings = np.genfromtxt(known_encodings_file, delimiter=',')
    else:
        known_encodings = []

    # #For Storing the name corresponding to the encoding
    people_file = Path(people_file_path)
    if people_file.is_file():
        people = np.genfromtxt(people_file, dtype='U',delimiter=',')
    else:
        people = []

    # Capture Video indefinitely
    video_capture = cv2.VideoCapture(0)

    original_width = video_capture.get(cv2.CAP_PROP_FRAME_WIDTH)
    original_height = video_capture.get(cv2.CAP_PROP_FRAME_HEIGHT)

    # Some important variables
    face_locations = []
    face_encodings = []
    face_names = []
    process_this_frame = True
    
    while True:

        # 
        #     1.) Capture the frame from the video.
        #     2.) Compress it to its 1/4th size for faster speed.
        #     3.) If this frame has to be processed, find face_location, face_encodings.
        #     4.) Match with the known_encodings and set the name for each face else Unknown
        #     5.) Add a border around face.
        #         if RED: 
        #             Criminal
        #         elif Pink:
        #             Missing person
        #         elif Pink:
        #             unverified or not authenticated
        #         elif GREEN:
        #             everything OK ;)
        #     6.) Show the frame 
        # 

        # Due to QR Code scanning, video element changes the size of video capture,
        # which also affected this process(don't know why) so to convert it to original size
        if video_capture.get(cv2.CAP_PROP_FRAME_WIDTH)!=original_width or video_capture.get(cv2.CAP_PROP_FRAME_HEIGHT)!= original_height:
            video_capture.set(cv2.CAP_PROP_FRAME_WIDTH, original_width)
            video_capture.set(cv2.CAP_PROP_FRAME_HEIGHT, original_height)
        ret, frame = video_capture.read()

        # Don't proceed further until camera is able to capture pics
        if not ret:
            continue
        #smaller frame 1/4th of original size
        small_frame = cv2.resize(frame, (0,0), fx=.25, fy=.25)

        if process_this_frame:
            #Find the face locations
            face_locations = face_recognition.face_locations(small_frame)
            #Find the face encodings 128 Dimensional!!
            face_encodings = face_recognition.face_encodings(small_frame, face_locations)

            face_names=[]
            face_type=[]
            face_number=[]
            other = 0 #Count of un-authorised people
            for face_encoding in face_encodings:
                match = face_recognition.compare_faces(known_encodings, face_encoding,tolerance=0.5)
                name = "Unknown"
                vtype = ""
                vnumber = ""
                
                #Find if this person is in the present people array
                for i in range(len(match)):
                    if match[i]:
                        
                        name = people[i][0]
                        vtype = people[i][1]
                        vnumber = people[i][2]
                        break
                # Change it, run the loop to find no. of Unknown
                if "Unknown" in name:
                    other += 1
                    name += str(other)
                face_names.append(name)
                face_type.append(vtype)
                face_number.append(vnumber)
            print(face_names, flush=True)
        process_this_frame = not process_this_frame

        # Display the border
        for (top, right, bottom, left),name,vtype,vnumber in zip(face_locations,face_names,face_type,face_number):

            # Scale up the coordinates by 4 to get face
            top *= 4
            right *= 4
            bottom *= 4
            left *= 4

            # Assuming person in authenticated
            color =  (0,255,0)  #GREEN
            if not authorised(name):
                # Unauthenticated person
                color = (255,0,0) #Blue
                directory = "./public/photos/" 
                filename = directory + time.strftime("%Y%m%d-%H%M%S") + ".jpeg"
                if not os.path.exists(directory + time.strftime("%Y%m%d-%H")):
                    os.makedirs(directory+ time.strftime("%Y%m%d-%H"))
                    crop_img = frame[(top):(bottom), (left):(right)]
                    cv2.imwrite(filename, crop_img)   
                    txt = "Hi, Identified at " + time.strftime("%d-%m-%Y %H:%M:%S") + ". Use the link to verify or report http://localhost:3000/load"
                    #send_email(filename,txt)
            else:
                if (vtype):
                    if vtype == "Missing":
                        color = (170,80,255) #Pink
                        Missing_txt = "Missing person found, ID :" + vnumber
                        print("Missing_txt: ",Missing_txt)
                        #send_email(filename,Missing_txt)
                    else:
                        color = (0,0,255) #RED

            # Display border
            cv2.rectangle(frame, (left,top), (right,bottom), color, 2)

            # Draw a label with name
            cv2.rectangle(frame, (left,bottom-35), (right, bottom), color, cv2.FILLED)
            font = cv2.FONT_HERSHEY_DUPLEX
            
            cv2.putText(frame, name,(left+6, bottom-6), font, 1.0, (255,255,255), 1)
            
        # Display the resulting image with borders and names
        cv2.imshow('Video', frame)

        # Hit 'q' on keyboard to quit
        if cv2.waitKey(100) == 27:
            break
            
    #Release handle to the webcam
    video_capture.release()
    cv2.closeAllWindows()


#start process
if __name__ == '__main__':
    main()
