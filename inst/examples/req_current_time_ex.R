# Fetch the current time
current_time <- req_current_time()

# Verify that current_time is a POSIXct object
class(current_time)

# Print current_time
current_time
