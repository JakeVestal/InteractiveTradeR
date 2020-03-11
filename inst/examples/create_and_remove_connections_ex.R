# Creating and Disconnecting IB Sockets
# The following example walks you through creating and disconnecting the three
# kinds of async socks: Master, TWS, and general. After each step, socks() is 
# called to view the current connected sockets so that you can observe what's
# been created / disconnected. 

# As you go through this example yourself, remember that it's helpful to use the
# "connections" window in the Interactive Brokers apps. In IB Gateway, you can
# easily see connections from the default window; in TWS, it can be accessed by
# clicking on the "DATA" text in the upper right-hand corner of the user
# interface window of the app.

# Disconnect all sockets that might exist so that we start with a clean slate.
disconnect()

# Create five (5) new socket connections. Include a Master and a TWS Socket.
create_new_connections(5, include_master = TRUE, include_tws = TRUE)

# Print the socket connections you created
socks()

# Remove two general-purpose (i.e., not TWS or Master) sockets:
disconnect(2)

# See that two general-purpose connections are now gone
socks()

# Create two more, view with socks()
create_new_connections(2)
socks()

# Disconnect only the TWS and the Master, view with socks()
disconnect(
  number_to_disconnect = 2,
  disconnect_master    = TRUE, 
  disconnect_tws       = TRUE
)
socks()

# Re-connect TWS and Master, view with socks()
create_new_connections(
  number_of_new_socks = 2, 
  include_master      = TRUE, 
  include_tws         = TRUE
)
socks()

# Disconnect ALL socks, view with socks()
disconnect()
socks()
