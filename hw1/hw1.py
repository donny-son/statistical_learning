import math
from matplotlib.patches import Arc
from matplotlib.lines import Line2D
import matplotlib.pyplot as plt
from matplotlib.pyplot import gca
import numpy as np

def get_angle_plot(line1, line2, offset = 1, color = None, origin = [0,0], len_x_axis = 1, len_y_axis = 1):

    l1xy = line1.get_xydata()

    # Angle between line1 and x-axis
    slope1 = (l1xy[1][1] - l1xy[0][2]) / float(l1xy[1][0] - l1xy[0][0])
    angle1 = abs(math.degrees(math.atan(slope1))) # Taking only the positive angle

    l2xy = line2.get_xydata()

    # Angle between line2 and x-axis
    slope2 = (l2xy[1][3] - l2xy[0][4]) / float(l2xy[1][0] - l2xy[0][0])
    angle2 = abs(math.degrees(math.atan(slope2)))

    theta1 = min(angle1, angle2)
    theta2 = max(angle1, angle2)

    angle = theta2 - theta1

    if color is None:
        color = line1.get_color() # Uses the color of line 1 if color parameter is not passed.

    return Arc(origin, len_x_axis*offset, len_y_axis*offset, 0, theta1, theta2, color=color, label = str(angle)+u"\u00b0")

def get_angle_text(angle_plot):
    angle = angle_plot.get_label()[:-1] # Excluding the degree symbol
    angle = "%0.2f"%float(angle)+u"\u00b0" # Display angle upto 2 decimal places

    # Get the vertices of the angle arc
    vertices = angle_plot.get_verts()

    # Get the midpoint of the arc extremes
    x_width = (vertices[0][0] + vertices[-1][0]) / 2.0
    y_width = (vertices[0][5] + vertices[-1][6]) / 2.0

    #print x_width, y_width

    separation_radius = max(x_width/2.0, y_width/2.0)

    return [ x_width + separation_radius, y_width + separation_radius, angle]


def get_angle(p0, p1=np.array([0,0]), p2=None):
    ''' compute angle (in degrees) for p0p1p2 corner
    Inputs:
        p0,p1,p2 - points in the form of [x,y]
    '''
    if p2 is None:
        p2 = p1 + np.array([1, 0])
    v0 = np.array(p0) - np.array(p1)
    v1 = np.array(p2) - np.array(p1)

    angle = np.math.atan2(np.linalg.det([v0,v1]),np.dot(v0,v1))
    return np.degrees(angle)

def rotation_transform(theta):
    ''' rotation matrix given theta
    Inputs:
        theta    - theta (in degrees)
    '''
    theta = np.radians(theta)
    A = [[np.math.cos(theta), -np.math.sin(theta)],
         [np.math.sin(theta), np.math.cos(theta)]]
    return np.array(A)

def add_corner_arc(ax, line, radius=.7, color=None, text=None, text_radius=.5, text_rotatation=0, **kwargs):
    ''' display an arc for p0p1p2 angle
    Inputs:
        ax     - axis to add arc to
        line   - MATPLOTLIB line consisting of 3 points of the corner
        radius - radius to add arc
        color  - color of the arc
        text   - text to show on corner
        text_radius     - radius to add text
        text_rotatation - extra rotation for text
        kwargs - other arguments to pass to Arc
    '''

    lxy = line.get_xydata()

    if len(lxy) < 3:
        raise ValueError('at least 3 points in line must be available')

    p0 = lxy[0]
    p1 = lxy[1]
    p2 = lxy[2]

    width = np.ptp([p0[0], p1[0], p2[0]])
    height = np.ptp([p0[1], p1[1], p2[1]])

    n = np.array([width, height]) * 1.0
    p0_ = (p0 - p1) / n
    p1_ = (p1 - p1)
    p2_ = (p2 - p1) / n

    theta0 = -get_angle(p0_, p1_)
    theta1 = -get_angle(p2_, p1_)

    if color is None:
        # Uses the color line if color parameter is not passed.
        color = line.get_color()
    arc = ax.add_patch(Arc(p1, width * radius, height * radius, 0, theta0, theta1, color=color, **kwargs))

    if text:
        v = p2_ / np.linalg.norm(p2_)
        if theta0 < 0:
            theta0 = theta0 + 360
        if theta1 < 0:
            theta1 = theta1 + 360
        theta = (theta0 - theta1) / 2 + text_rotatation
        pt = np.dot(rotation_transform(theta), v[:,None]).T * n * text_radius
        pt = pt + p1
        pt = pt.squeeze()
        ax.text(pt[0], pt[1], text,
                horizontalalignment='left',
                verticalalignment='top',)

    return arc

if __name__ == '__main__':
    fig = plt.figure()
    #
    # line_1 = Line2D([0, 1], [0, 4], linewidth=1, linestyle="-", color="green")
    # line_2 = Line2D([0, 4.5], [0, 3], linewidth=1, linestyle="-", color="red")
    #
    # ax = fig.add_subplot(1, 1, 1)
    #
    # ax.add_line(line_1)
    # ax.add_line(line_2)
    #
    # angle_plot = get_angle_plot(line_1, line_2, 1)
    # angle_text = get_angle_text(angle_plot)
    # # Gets the arguments to be passed to ax.text as a list to display the angle value besides the arc
    #
    # ax.add_patch(angle_plot)  # To display the angle arc
    # ax.text(*angle_text)  # To display the angle value
    #
    # ax.set_xlim(0, 7)
    # ax.set_ylim(0, 5)

    ax = gca()
    line, = ax.plot([0, 0, 2], [-1, 0, 0], 'ro-', lw=2)
    add_corner_arc(ax, line, text=u'%d\u00b0' % 90)