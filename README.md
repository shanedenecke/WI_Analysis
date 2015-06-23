# WI_Analysis
A comprehensive analysis pipeline to produce analyze movement data generated by the Wiggle Index

The first set of scripts is a modification of the script published by Preston et. al (2015) on worm motion. The function is to measure total motion of larvae in a well over a given interval. See the Preston et. al (2015) and Denecke et. al (2015) for details. Where the Denecke script differs is that it takes videos which have 4 individual wells and crops each one out and analyzes it separately. The cropping is done by pre set parameters so you may have to guess and check a few times :\. 
There are 4 of these scripts for each of the 4 wells in one shot. It will spit you out an excelish document with 2 columns "Image Name" and "Wiggle Index". The second will be your movement values and the first will be the name of each repitition that it inherited from your original video. 



The naming is important

Essentially You want the following separated by "."
1) Genotype
2) Dose
3) Time
4) Plate
5) Date
6) Junk given to you by Video Jpg converter (https://www.dvdvideosoft.com/products/dvd/Free-Video-to-JPG-Converter.htm)
7) Well Position

Ex:
You should automatically get something like this
(Da1_M4).(0ppm).(0min).(plate1).(Imidacloprid).(19.5.15) (5-19-2015 11-30-26 AM)LowLeft.tif

from inputting the video 
(Da1_M4).(0ppm).(0min).(plate1).(Imidacloprid).(19.5.15).MTS





Then the analysis begeins

The script will present you with a series of options:

First it will ask you how you want to compare the your data
-Doses: Puts multiple doses of one genotype onto the same graph
-Genotypes: Puts multiple genotypes at one dose on the same graph
-GLM: Generalized Lienar Model. Spits out bar graphs that compare coeficients of generalized linear models for each genoytpe at a given dose

Then it will ask you for a time frame (Arbitrary)
-Short: 0-30 minutes
-Medium: 0-120 minutes
-Long: 0-240 minutes

Then it will see what type of Data you want plotted
-Relative- Corrects data for starting motion (Does not work with GLM)
-Raw- Plots Raw WI values

The script then generates all of the graphs you selected and dumps them in subfolders.

Feel free to edit at will
