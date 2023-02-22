import serial
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import sys, time, math

# GUI libraries
import tkinter as tk
from tkinter import simpledialog
from PIL import Image, ImageTk
import keyboard

# Best programming practice: Keep all variables global (joke)
xsize = 300
# preheat period
HEAT_TIME_PREHEAT = 11#0#0
HEAT_TIME_REFLOW = 10#0#0 
COOLING_TIME = 7#5#0

Com_Port = "COM4"
Soak_temp = 0
Soak_time = 0
Reflow_temp = 0
Reflow_time = 0
ydata2 = []

ROOT = tk.Tk()
my_label = None

def main():
    ROOT.geometry("700x600")
    ROOT.title('Rizzlers Strip Chart')
    ROOT.configure(bg='white', cursor='hand2')

    img = ImageTk.PhotoImage(Image.open("back.png"))
    my_label = tk.Label(image=img)
    my_label.place(x=-2, y=0)

    btn_quit = tk.Button(ROOT, text="Exit", command=ROOT.quit, activebackground='red', activeforeground="white")
    btn_quit.place(x=100, y=500, width=200, height=45)

    btn_2 = tk.Button(ROOT, text="Start!", command=askInputs, activebackground='green', activeforeground="white")
    btn_2.place(x=360, y=500, width=200, height=45)
    
    # Loop - Place relevant code above this line
    ROOT.mainloop()

def waitingScreen():  
    if not keyboard.is_pressed("a"):
        ROOT.after(50, waitingScreen)
    else:
        ROOT.withdraw()

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

def askInputs():
    ROOT.withdraw() # Hides previous window
    global Com_Port
    global Soak_temp
    global Soak_time
    global Reflow_temp
    global Reflow_time

    Com_Port = simpledialog.askstring(title="ComPort", prompt="Please select a COM Port:")
    Soak_temp = simpledialog.askinteger(title="SoakTemp", prompt="Please enter the Soak Temperature:")
    Soak_time = simpledialog.askinteger(title="SoakTime", prompt="Please enter the Soak Time:")
    Reflow_temp = simpledialog.askinteger(title="ReflowTemp", prompt="Please enter the Reflow Temperatrure")
    Reflow_time = simpledialog.askinteger(title="ReflowTime", prompt="Please enter the Reflow Time")

    ROOT.deiconify()

    # Waiting screen and delete previous elements
    for ele in ROOT.winfo_children():
        ele.destroy()
    
    img = ImageTk.PhotoImage(Image.open("waitingScreen.png"))
    my_label = tk.Label(image=img)
    my_label.image = img
    my_label.place(x=-2, y=0)

    """Not the best solution, but since we already have the inputs we need,
    we can now do the plotting. Important Note: Do not use Root.quit() until before we are done using
    the user inputed parameters (like Soak_temp)"""

    waitingScreen()

if __name__ == "__main__":
    main()