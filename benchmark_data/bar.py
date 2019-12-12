import numpy as np
import matplotlib
matplotlib.use('PDF')
import matplotlib.pyplot as plt
n_groups = 5
sp24 = [9.4654913738, 17.651392193, 10.4235939541, 3.8743560846, 16.9296645181]
sp12 = [8.11894673537, 12.4723346379, 7.11374757385, 3.27217997733, 11.4749782561]
fig, ax = plt.subplots(1,1)
index = np.arange(n_groups)
bar_width = 0.35
rects1 = ax.bar(index, sp12, bar_width, color='g', label='12 C')
rects2 = ax.bar(index + bar_width, sp24, bar_width, color='b', label='2 x 12 C')
ax.set_ylabel('Speedups')
plt.xticks(index + bar_width/2, ('dot', 'fft', 'ms', 'qs', 'scalar'))
ax.legend()
fig.savefig("speedups_p", dpi=300)
plt.close(fig)
