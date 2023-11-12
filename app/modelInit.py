import torch
import torch.nn as nn 
import torch.optim as optim
from app.DQN import DQN
# Define the DQN architecture
# Assuming 3 inputs (heart rate, oxygen saturation, sleep data) and 5 actions
state_size = 6
action_size = 3

# Initialize the DQN model
dqn_model = DQN(state_size, action_size)

# Define the optimizer (not used in this example, but typically needed for training)
optimizer = optim.Adam(dqn_model.parameters(), lr=0.001)

# Save the model
torch.save(dqn_model, 'biohacker.pt')
