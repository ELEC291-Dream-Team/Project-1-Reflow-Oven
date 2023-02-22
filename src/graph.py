import serial
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import sys, time, math

# For Simple GUI implementation
import tkinter as tk
from tkinter import simpledialog

ROOT = tk.Tk()
ROOT.withdraw()

xsize = 300

# defining parameters
# preheat period
HEAT_TIME_PREHEAT = 1100
HEAT_TIME_REFLOW = 1000 
COOLING_TIME = 750 


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
initial_read  = ser.readline()
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
def expected(milli):
    if milli < HEAT_TIME_PREHEAT:
        return milli * Reflow_temp/HEAT_TIME_PREHEAT
    TOTAL_TIME = HEAT_TIME_PREHEAT + Soak_time
    if milli < TOTAL_TIME:
        return Soak_temp
    TOTAL_TIME += HEAT_TIME_REFLOW
    if milli < TOTAL_TIME:
        return (milli-TOTAL_TIME-HEAT_TIME_REFLOW)*(Soak_temp - Reflow_temp) / HEAT_TIME_REFLOW
    TOTAL_TIME += Soak_time
    if milli < TOTAL_TIME:
        return Soak_temp
    return ( milli - TOTAL_TIME ) * (Reflow_temp-25) / (COOLING_TIME)

def run(data):
    global ydata2
    t,y = data
    if t>-1:
        xdata_raw.append(t)
        ydata_raw.append(y)
        yexp = expected(t)
        print(yexp)
        ydata2.append(y)
        if t>xsize: # Scroll to the left.
            ax.set_xlim(t-xsize, t)
        xdata = xdata_raw[-xsize:]
        ydata = ydata_raw[-xsize:]
        ydata2 = ydata2[-xsize:]
        line.set_data(xdata, ydata)
        line2.set_data(xdata, ydata2)
    return line, line2


def on_close_figure(event):
    sys.exit(0)

""""
Soak_temp = int(input("Soak Temperature:\t"))
Soak_time = int(input("Soak Time:\t"))
Reflow_temp = int(input("Reflow temperature:\t"))
Reflow_time = int(input("Reflow Time:\t"))

An example of a simple dialog box for each input
(Window closes after each var is inputed)
"""""

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