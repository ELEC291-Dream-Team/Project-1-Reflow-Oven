import serial
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import sys, time, math

#import keyboard

# For Simple GUI implementation
import tkinter as tk
from tkinter import simpledialog

ROOT = tk.Tk()
ROOT.withdraw()

xsize = 300

# defining parameters
# preheat period
HEAT_TIME_PREHEAT = 11#0#0
HEAT_TIME_REFLOW = 10#0#0 
COOLING_TIME = 7#5#0 



Com_Port = simpledialog.askstring(title="ComPort", prompt="Please select a COMPort:")

# configure the serial port
ser = serial.Serial(
    port = Com_Port,
    baudrate = 115200,
    parity = serial.PARITY_NONE,
    stopbits = serial.STOPBITS_TWO,
    bytesize = serial.EIGHTBITS)
ser.isOpen()

# initial read
initial_read  = ser.readline().decode('utf-8')
init_temp = int(initial_read)
def data_gen():
    t = data_gen.t
    while True:
        if t == 0:
            temp = int(initial_read)
        else:
            ser.reset_input_buffer()
            temp = int(ser.readline())
        #print(temp)
        t += 1
        
        yield t, temp

def expected(milli, Soak_temp, Soak_time, Reflow_temp):
    if milli < HEAT_TIME_PREHEAT:
        return milli * (Soak_temp-init_temp)/HEAT_TIME_PREHEAT + init_temp
    TOTAL_TIME = HEAT_TIME_PREHEAT + Soak_time
    if milli < TOTAL_TIME:
        return Soak_temp
    TOTAL_TIME += HEAT_TIME_REFLOW
    if milli < TOTAL_TIME:
        return (milli-TOTAL_TIME+HEAT_TIME_REFLOW)*(Reflow_temp - Soak_temp) / HEAT_TIME_REFLOW + Soak_temp
    TOTAL_TIME += Reflow_time
    if milli < TOTAL_TIME:
        return Reflow_temp
    TOTAL_TIME += COOLING_TIME
    if milli < TOTAL_TIME:
        return ( milli - TOTAL_TIME + COOLING_TIME ) * (init_temp-Reflow_temp) / (COOLING_TIME) + Reflow_temp
    return init_temp

    

def run(data):
    global ydata2
    t,y = data
    if t>-1:
        xdata_raw.append(t)
        ydata_raw.append(y)
        yexp = expected(t, Soak_temp, Soak_time, Reflow_temp)
        ydata2.append(yexp)
        if t>xsize:
            ax.set_xlim(t-xsize, t)
        xdata = xdata_raw[-xsize:]
        ydata = ydata_raw[-xsize:]
        ydata2 = ydata2[-xsize:]
        line.set_data(xdata, ydata)
        line2.set_data(xdata, ydata2)
        if t >= HEAT_TIME_PREHEAT + Soak_time + HEAT_TIME_REFLOW + Reflow_time + COOLING_TIME + 5:
            plt.pause(0.01)  # pause for a brief moment to update the chart
            plt.close(fig)  # close the chart window
            return None
    return line, line2



def on_close_figure(event):
    print('Closed Figure!')
    max_over = 0
    time_over = 0
    max_under = 0
    time_under = 0
    for i in range(HEAT_TIME_PREHEAT + Soak_time + HEAT_TIME_REFLOW + Reflow_time + COOLING_TIME + 5):
        if ydata_raw[i] - ydata2[i] > max_over:
            max_over = ydata_raw[i] - ydata2[i]
            time_over = i
        if ydata2[i] - ydata_raw[i] > max_under:
            max_under = ydata2[i] - ydata_raw[i]
            time_under = i
    print("Max over: " + str(max_over) + " at " + str(time_over/10)+ " seconds")
    print("Max under: " + str(max_under) + " at " + str(time_under/10) + " seconds")
    sys.exit(0)

Soak_temp = int(simpledialog.askstring(title="SoakTemp", prompt="Please enter the Soak Temperature:"))
Soak_time = int(simpledialog.askstring(title="SoakTime", prompt="Please enter the Soak Time:"))
Reflow_temp = int(simpledialog.askstring(title="ReflowTemp", prompt="Please enter the Reflow Temperatrure"))
Reflow_time = int(simpledialog.askstring(title="ReflowTime", prompt="Please enter the Reflow Time"))



data_gen.t = -1
fig = plt.figure()
fig.canvas.mpl_connect('close_event', on_close_figure)
ax = fig.add_subplot(111)
line, = ax.plot([], [], lw=2, color='blue')
line2, = ax.plot([], [], lw=2, color='red') # add a second line with a different color
ax.set_ylim(0, 100)
ax.set_xlim(0, xsize)
ax.grid()
xdata_raw, ydata_raw, ydata2 = [], [], []


# Important: Although blit=True makes graphing faster, we need blit=False to prevent
# spurious lines to appear when resizing the stripchart.
ani = animation.FuncAnimation(fig, run, data_gen, blit=False, interval=50, repeat=False)
plt.show()