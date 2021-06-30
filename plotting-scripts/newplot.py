import os
import json
import matplotlib.pyplot as plt

avgfiles = [filename for filename in os.listdir('combined') if filename.startswith("combined-avg")]

lines=[]

lines.append(("SeqSolve",176,[0,18],[0,1178]))
lines.append(("CVC4",3200,[0,3200],[0,1128]))
lines.append(("Z3Str3",13527,[0,13527],[0,947]))
lines.append(("Norn",12783,[0,12783],[0,883]))
lines.append(("Z3Str2",18,[0,18],[0,465]))
lines.append(("Trau",5223,[0,5223],[0,1081]))
lines.append(("Sloth",7486,[0,7486],[0,858]))

tools=[]
linestyles = [':','--','-.','--','-','-.',':']
markers = ['s','o','X','d','^','h','*']
dashcapstyles = ['butt','round','projecting','butt','round','projecting','butt'] 
i=0
for l in lines:
    plt.plot(l[3], l[2], label=l[0],ls=linestyles[i],marker=markers[i],markevery=0.1,ms=10,dash_capstyle=dashcapstyles[i],lw=2)
    i += 1

plt.grid(True, color='black', ls=':', lw=1, zorder=1)
plt.ylim(ymin=0, ymax=14000)
plt.xlim(xmin=0, xmax=1200)
plt.ylabel('Expected Time in seconds', size = 10)
plt.xlabel('Expected Number of Problems Solved', size = 10)
plt.xticks (fontsize=5)
plt.yticks (fontsize=5)

plt.legend(loc="upper left", prop={'size': 10})

ax = plt.gca()
#sets the ratio to 5
#ax.set_aspect(0.03)
#ax.set_aspect(3)
plt.show()
#plt.savefig('expected_flipped.pdf', bbox_inches='tight')
