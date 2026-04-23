FROM ghcr.io/microbiome/miadash:latest

LABEL authors="giulio.benedetti@utu.fi" \
    description="Docker image containing the miaDash package."

# Set the working directory to /app
WORKDIR /app

# Copy the current directory contents into the container at /app
ADD . /app

# Make port 1234 available to the world outside this container
EXPOSE 1234

# Run the script that launches the Shiny app
CMD ["R", "-e", "library(miaDash); app <- miaDash(); shiny::runApp(app, port = 1234, host = '0.0.0.0')"]
