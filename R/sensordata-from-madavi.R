library(RCurl)
library(ggplot2)
require(dplyr)


getSensorData <- function(sensorstring, sensorlocation, datestring="") {
    baseURL <- 'https://www.madavi.de/sensor/data/data'

    if (datestring == "") {
        Date <- Sys.Date()
        datestring <- format(Date, '%Y-%m-%d')
    }

    url2fetch <- paste(baseURL, sensorstring, datestring, sep='-')
    url2fetch <- paste(url2fetch, 'csv', sep='.')
    url_content <- getURL(url2fetch, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

    raw_sensor_data <- read.csv(textConnection(url_content), sep=";", skip=1, header=FALSE)

    names(raw_sensor_data)[names(raw_sensor_data) == 'V1'] <- 'datetime'
    names(raw_sensor_data)[names(raw_sensor_data) == 'V8'] <- 'PM10'
    names(raw_sensor_data)[names(raw_sensor_data) == 'V9'] <- 'PM25'

    raw_sensor_data$datetime  <-as.POSIXct(raw_sensor_data$datetime,format="%Y/%m/%d %H:%M:%S")

    head(raw_sensor_data)

    returned_data <- data.frame(sensorstring, sensorlocation, raw_sensor_data$datetime, raw_sensor_data$PM10, raw_sensor_data$PM25)
    names(returned_data) <- c("Sensor", "Location", "DateTime", "PM10", "PM25")

    return(returned_data)
}


data <- getSensorData("esp8266-906088", "Bridgemere")
data <- rbind(data, getSensorData("esp8266-5016171", "Town Centre"))
data <- rbind(data, getSensorData("esp8266-5015194", "Old Town"))
data <- rbind(data, getSensorData("esp8266-5015649", "Stone Cross"))

#ggplot(data, aes(x = DateTime, y = PM10)) + geom_line(aes(color = Location)) + geom_smooth() + ggtitle("PM10 Pollution in Eastbourne")


# Work out 10 minute averages for the data so it's less jumpy.
avdata <- data %>%
  group_by(Location, DateTime = cut(DateTime, breaks="10 min")) %>%
  summarize(PM10 = mean(PM10))
avdata$DateTime <-as.POSIXct(avdata$DateTime,format="%Y-%m-%d %H:%M:%S")

# Now plot it, uncomment for limit lines if needed.
ggplot(avdata, aes(x = DateTime, y = PM10)) +
geom_line(aes(color = Location)) +
#geom_hline(yintercept = 40, color="red") +
#annotate("text", min(avdata$DateTime), 40, vjust = -1, label = "UK Limit") +
#geom_hline(yintercept = 20, color="pink") +
#annotate("text", min(avdata$DateTime), 20, vjust = -1, label = "WHO Limit") +
ylab("PM10 µg/m3") +
labs(title = "PM10 Pollution in Eastbourne",
     caption = "@eastbourneair")
