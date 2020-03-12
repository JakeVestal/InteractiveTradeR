# Create some socket connections
create_new_connections(5, include_master = TRUE, include_tws = TRUE)

# Fetch the next valid ID on the master socket:
req_ids("master")

# Fetch the next valid ID on the TWS socket:
req_ids("tws")

# Note that opening orders within TWS itself does not affect the next valid TWS
# id -- only orders submitted via the API will cause it to increment.

# Repeat for the other sockets:
req_ids("sock_1")
req_ids("sock_2")
req_ids("sock_3")

# Calling req_ids() in sync mode will work, but isn't of much use because
# because, by definition, the socket isn't specified:
req_ids()
