# AIM-Lotic-IndicatorCalculation
Calculation of Lotic AIM indicators from raw data.

INSTRUCTIONS
1. Download or determine path for accessing Lotic AIM data
2. Create R Project within folder containing these scripts
3. Open and configure "config" file. This requires determining how you are accessing the data (e.g file geodatabase, rest service).Save. This is the only file you should need to edit
4. Run the “Step_01_Initialization” script. This sources the config file you just edited.
5. Source "Step_02_IndicatorCalc script". This will source all of the individual scripts calculating indicators and then combine and format them.