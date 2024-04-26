;;; biome-api-data.el --- Open Meteo API description  -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Korytov Pavel

;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Homepage: https://github.com/SqrtMinusOne/biome

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Open Meteo API data, parsed by `biome-api-parse--generate'.

;;; Code:
(defconst biome-api-data
  '(("Weather Forecast"
     (:name . "Weather Forecast")
     (:url . "https://open-meteo.com/en/docs")
     (:description . "Seamless integration of high-resolution weather models with up 16 days forecast")
     (:key . "ww")
     (:sections
      ((:name . "Select Coordinates and Time")
       (:fields
        ("end_date"
         (:name . "End date")
         (:type . date))
        ("start_date"
         (:name . "Start date")
         (:type . date))
        ("forecast_days"
         (:name . "Forecast days")
         (:type . number)
         (:min . 0)
         (:max . 16))
        ("past_days"
         (:name . "Past days")
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))
        ("timezone"
         (:name . "Timezone")
         (:type . timezone))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Additional Variables And Options")
         (:fields
          ("uv_index"
           (:name . "UV Index")
           (:type . checkbox))
          ("uv_index_clear_sky"
           (:name . "UV Index Clear Sky")
           (:type . checkbox))
          ("is_day"
           (:name . "Is Day or Night")
           (:type . checkbox))
          ("cape"
           (:name . "CAPE")
           (:type . checkbox))
          ("freezing_level_height"
           (:name . "Freezing Level Height")
           (:type . checkbox))
          ("sunshine_duration"
           (:name . "Sunshine Duration")
           (:type . checkbox))
          ("forecast_hours"
           (:name . "Forecast Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))
          ("past_hours"
           (:name . "Past Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))))
        ((:name . "Solar Radiation Variables")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation GHI")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation DHI")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("global_tilted_irradiance"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation"
           (:name . "Terrestrial Solar Radiation")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation GHI (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation DHI (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox))
          ("global_tilted_irradiance_instant"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation_instant"
           (:name . "Terrestrial Solar Radiation (Instant)")
           (:type . checkbox))
          ("tilt"
           (:name . "Panel Tilt (0° horizontal)")
           (:type . number))
          ("azimuth"
           (:name . "Panel Azimuth (0° S, -90° E, 90° W)")
           (:type . number))))
        ((:name . "Pressure Level Variables")
         (:children
          ((:name . "Temperature")
           (:fields
            ("temperature_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("temperature_975hPa"
             (:name . "975 hPa (320 m)")
             (:type . checkbox))
            ("temperature_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("temperature_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("temperature_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("temperature_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("temperature_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("temperature_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("temperature_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("temperature_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("temperature_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("temperature_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("temperature_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("temperature_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("temperature_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("temperature_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("temperature_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("temperature_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("temperature_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))))
          ((:name . "Relative Humidity")
           (:fields
            ("relative_humidity_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("relative_humidity_975hPa"
             (:name . "975 hPa (320 m)")
             (:type . checkbox))
            ("relative_humidity_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("relative_humidity_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("relative_humidity_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("relative_humidity_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("relative_humidity_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("relative_humidity_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("relative_humidity_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("relative_humidity_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("relative_humidity_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("relative_humidity_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("relative_humidity_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("relative_humidity_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("relative_humidity_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("relative_humidity_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("relative_humidity_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("relative_humidity_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("relative_humidity_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))))
          ((:name . "Cloud cover")
           (:fields
            ("cloud_cover_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("cloud_cover_975hPa"
             (:name . "975 hPa (320 m)")
             (:type . checkbox))
            ("cloud_cover_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("cloud_cover_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("cloud_cover_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("cloud_cover_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("cloud_cover_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("cloud_cover_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("cloud_cover_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("cloud_cover_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("cloud_cover_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("cloud_cover_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("cloud_cover_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("cloud_cover_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("cloud_cover_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("cloud_cover_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("cloud_cover_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("cloud_cover_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("cloud_cover_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))))
          ((:name . "Wind Speed")
           (:fields
            ("windspeed_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("windspeed_975hPa"
             (:name . "975 hPa (320 m)")
             (:type . checkbox))
            ("windspeed_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("windspeed_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("windspeed_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("windspeed_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("windspeed_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("windspeed_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("windspeed_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("windspeed_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("windspeed_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("windspeed_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("windspeed_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("windspeed_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("windspeed_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("windspeed_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("windspeed_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("windspeed_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("windspeed_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))))
          ((:name . "Wind Direction")
           (:fields
            ("winddirection_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("winddirection_975hPa"
             (:name . "975 hPa (320 m)")
             (:type . checkbox))
            ("winddirection_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("winddirection_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("winddirection_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("winddirection_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("winddirection_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("winddirection_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("winddirection_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("winddirection_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("winddirection_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("winddirection_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("winddirection_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("winddirection_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("winddirection_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("winddirection_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("winddirection_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("winddirection_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("winddirection_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))))
          ((:name . "Geopotential Height")
           (:fields
            ("geopotential_height_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("geopotential_height_975hPa"
             (:name . "975 hPa (320 m)")
             (:type . checkbox))
            ("geopotential_height_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("geopotential_height_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("geopotential_height_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("geopotential_height_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("geopotential_height_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("geopotential_height_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("geopotential_height_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("geopotential_height_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("geopotential_height_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("geopotential_height_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("geopotential_height_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("geopotential_height_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("geopotential_height_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("geopotential_height_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("geopotential_height_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("geopotential_height_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("geopotential_height_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox)))))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dew_point_2m"
         (:name . "Dewpoint (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("precipitation_probability"
         (:name . "Precipitation Probability")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation (rain + showers + snow)")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("showers"
         (:name . "Showers")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("snow_depth"
         (:name . "Snow Depth")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloud_cover"
         (:name . "Cloud cover Total")
         (:type . checkbox))
        ("cloud_cover_low"
         (:name . "Cloud cover Low")
         (:type . checkbox))
        ("cloud_cover_mid"
         (:name . "Cloud cover Mid")
         (:type . checkbox))
        ("cloud_cover_high"
         (:name . "Cloud cover High")
         (:type . checkbox))
        ("visibility"
         (:name . "Visibility")
         (:type . checkbox))
        ("evapotranspiration"
         (:name . "Evapotranspiration")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))
        ("vapour_pressure_deficit"
         (:name . "Vapour Pressure Deficit")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_speed_80m"
         (:name . "Wind Speed (80 m)")
         (:type . checkbox))
        ("wind_speed_120m"
         (:name . "Wind Speed (120 m)")
         (:type . checkbox))
        ("wind_speed_180m"
         (:name . "Wind Speed (180 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_direction_80m"
         (:name . "Wind Direction (80 m)")
         (:type . checkbox))
        ("wind_direction_120m"
         (:name . "Wind Direction (120 m)")
         (:type . checkbox))
        ("wind_direction_180m"
         (:name . "Wind Direction (180 m)")
         (:type . checkbox))
        ("wind_gusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))
        ("temperature_80m"
         (:name . "Temperature (80 m)")
         (:type . checkbox))
        ("temperature_120m"
         (:name . "Temperature (120 m)")
         (:type . checkbox))
        ("temperature_180m"
         (:name . "Temperature (180 m)")
         (:type . checkbox))
        ("soil_temperature_0cm"
         (:name . "Soil Temperature (0 cm)")
         (:type . checkbox))
        ("soil_temperature_6cm"
         (:name . "Soil Temperature (6 cm)")
         (:type . checkbox))
        ("soil_temperature_18cm"
         (:name . "Soil Temperature (18 cm)")
         (:type . checkbox))
        ("soil_temperature_54cm"
         (:name . "Soil Temperature (54 cm)")
         (:type . checkbox))
        ("soil_moisture_0_to_1cm"
         (:name . "Soil Moisture (0-1 cm)")
         (:type . checkbox))
        ("soil_moisture_1_to_3cm"
         (:name . "Soil Moisture (1-3 cm)")
         (:type . checkbox))
        ("soil_moisture_3_to_9cm"
         (:name . "Soil Moisture (3-9 cm)")
         (:type . checkbox))
        ("soil_moisture_9_to_27cm"
         (:name . "Soil Moisture (9-27 cm)")
         (:type . checkbox))
        ("soil_moisture_27_to_81cm"
         (:name . "Soil Moisture (27-81 cm)")
         (:type . checkbox))))
      ((:param . "daily")
       (:name . "Daily Weather Variables")
       (:fields
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("temperature_2m_max"
         (:name . "Maximum Temperature (2 m)")
         (:type . checkbox))
        ("temperature_2m_min"
         (:name . "Minimum Temperature (2 m)")
         (:type . checkbox))
        ("apparent_temperature_max"
         (:name . "Maximum Apparent Temperature (2 m)")
         (:type . checkbox))
        ("apparent_temperature_min"
         (:name . "Minimum Apparent Temperature (2 m)")
         (:type . checkbox))
        ("sunrise"
         (:name . "Sunrise")
         (:type . checkbox))
        ("sunset"
         (:name . "Sunset")
         (:type . checkbox))
        ("daylight_duration"
         (:name . "Daylight Duration")
         (:type . checkbox))
        ("sunshine_duration"
         (:name . "Sunshine Duration")
         (:type . checkbox))
        ("uv_index_max"
         (:name . "UV Index")
         (:type . checkbox))
        ("uv_index_clear_sky_max"
         (:name . "UV Index Clear Sky")
         (:type . checkbox))
        ("precipitation_sum"
         (:name . "Precipitation Sum")
         (:type . checkbox))
        ("rain_sum"
         (:name . "Rain Sum")
         (:type . checkbox))
        ("showers_sum"
         (:name . "Showers Sum")
         (:type . checkbox))
        ("snowfall_sum"
         (:name . "Snowfall Sum")
         (:type . checkbox))
        ("precipitation_hours"
         (:name . "Precipitation Hours")
         (:type . checkbox))
        ("precipitation_probability_max"
         (:name . "Precipitation Probability Max")
         (:type . checkbox))
        ("wind_speed_10m_max"
         (:name . "Maximum Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_gusts_10m_max"
         (:name . "Maximum Wind Gusts (10 m)")
         (:type . checkbox))
        ("wind_direction_10m_dominant"
         (:name . "Dominant Wind Direction (10 m)")
         (:type . checkbox))
        ("shortwave_radiation_sum"
         (:name . "Shortwave Radiation Sum")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))))
      ((:param . "minutely_15")
       (:name . "15-Minutely Weather Variables")
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dew_point_2m"
         (:name . "Dewpoint (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation (rain + showers + snow)")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("snowfall_height"
         (:name . "Snowfall Height")
         (:type . checkbox))
        ("freezing_level_height"
         (:name . "Freezing Level Height")
         (:type . checkbox))
        ("sunshine_duration"
         (:name . "Sunshine Duration")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_speed_80m"
         (:name . "Wind Speed (80 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_direction_80m"
         (:name . "Wind Direction (80 m)")
         (:type . checkbox))
        ("wind_gusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))
        ("visibility"
         (:name . "Visibility")
         (:type . checkbox))
        ("cape"
         (:name . "CAPE")
         (:type . checkbox))
        ("lightning_potential"
         (:name . "Lightning Potential Index LPI")
         (:type . checkbox))
        ("is_day"
         (:name . "Is Day or Night")
         (:type . checkbox))
        ("shortwave_radiation"
         (:name . "Shortwave Solar Radiation GHI")
         (:type . checkbox))
        ("direct_radiation"
         (:name . "Direct Solar Radiation")
         (:type . checkbox))
        ("diffuse_radiation"
         (:name . "Diffuse Solar Radiation DHI")
         (:type . checkbox))
        ("direct_normal_irradiance"
         (:name . "Direct Normal Irradiance DNI")
         (:type . checkbox))
        ("global_tilted_irradiance"
         (:name . "Global Tilted Radiation GTI")
         (:type . checkbox))
        ("terrestrial_radiation"
         (:name . "Terrestrial Solar Radiation")
         (:type . checkbox))
        ("shortwave_radiation_instant"
         (:name . "Shortwave Solar Radiation GHI (Instant)")
         (:type . checkbox))
        ("direct_radiation_instant"
         (:name . "Direct Solar Radiation (Instant)")
         (:type . checkbox))
        ("diffuse_radiation_instant"
         (:name . "Diffuse Solar Radiation DHI (Instant)")
         (:type . checkbox))
        ("direct_normal_irradiance_instant"
         (:name . "Direct Normal Irradiance DNI (Instant)")
         (:type . checkbox))
        ("global_tilted_irradiance_instant"
         (:name . "Global Tilted Radiation GTI")
         (:type . checkbox))
        ("terrestrial_radiation_instant"
         (:name . "Terrestrial Solar Radiation (Instant)")
         (:type . checkbox))
        ("forecast_minutely_15"
         (:name . "Forecast Minutely 15")
         (:type . select)
         (:options
          ("4" . "1 hour")
          ("24" . "6 hours")
          ("48" . "12 hours")
          ("96" . "24 hours")))
        ("past_minutely_15"
         (:name . "Past Minutely 15")
         (:type . select)
         (:options
          ("1" . "1 hour")
          ("6" . "6 hours")
          ("12" . "12 hours")
          ("24" . "24 hours")))))
      ((:param . "current")
       (:name . "Current Weather")
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("is_day"
         (:name . "Is Day or Night")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("showers"
         (:name . "Showers")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("cloud_cover"
         (:name . "Cloud cover Total")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_gusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))))
      ((:name . "Settings")
       (:fields
        ("elevation"
         (:name . "Elevation")
         (:type . float))
        ("temperature_unit"
         (:name . "Temperature Unit")
         (:type . select)
         (:options
          ("celsius" . "Celsius °C")
          ("fahrenheit" . "Fahrenheit °F")))
        ("wind_speed_unit"
         (:name . "Wind Speed Unit")
         (:type . select)
         (:options
          ("kmh" . "Km/h")
          ("ms" . "m/s")
          ("mph" . "Mph")
          ("kn" . "Knots")))
        ("precipitation_unit"
         (:name . "Precipitation Unit")
         (:type . select)
         (:options
          ("mm" . "Millimeter")
          ("inch" . "Inch")))
        ("timeformat"
         (:name . "Timeformat")
         (:type . select)
         (:options
          ("iso8601" . "ISO 8601 (e.g. 2022-12-31)")
          ("unixtime" . "Unix timestamp")))))
      ((:param . "models")
       (:name . "Weather models")
       (:fields
        ("best_match"
         (:name . "Best match")
         (:type . checkbox))
        ("ecmwf_ifs04"
         (:name . "ECMWF IFS 0.4°")
         (:type . checkbox))
        ("ecmwf_ifs025"
         (:name . "ECMWF IFS 0.25°")
         (:type . checkbox))
        ("ecmwf_aifs025"
         (:name . "ECMWF AIFS 0.25°")
         (:type . checkbox))
        ("cma_grapes_global"
         (:name . "CMA GRAPES Global")
         (:type . checkbox))
        ("bom_access_global"
         (:name . "BOM ACCESS Global")
         (:type . checkbox))
        ("metno_nordic"
         (:name . "MET Norway Nordic")
         (:type . checkbox))
        ("gfs_seamless"
         (:name . "GFS Seamless")
         (:type . checkbox))
        ("gfs_global"
         (:name . "GFS Global")
         (:type . checkbox))
        ("gfs_hrrr"
         (:name . "GFS HRRR")
         (:type . checkbox))
        ("gfs_graphcast025"
         (:name . "GFS GraphCast")
         (:type . checkbox))
        ("jma_seamless"
         (:name . "JMA Seamless")
         (:type . checkbox))
        ("jma_msm"
         (:name . "JMA MSM")
         (:type . checkbox))
        ("jma_gsm"
         (:name . "JMA GSM")
         (:type . checkbox))
        ("icon_seamless"
         (:name . "DWD ICON Seamless")
         (:type . checkbox))
        ("icon_global"
         (:name . "DWD ICON Global")
         (:type . checkbox))
        ("icon_eu"
         (:name . "DWD ICON EU")
         (:type . checkbox))
        ("icon_d2"
         (:name . "DWD ICON D2")
         (:type . checkbox))
        ("gem_seamless"
         (:name . "GEM Seamless")
         (:type . checkbox))
        ("gem_global"
         (:name . "GEM Global")
         (:type . checkbox))
        ("gem_regional"
         (:name . "GEM Regional")
         (:type . checkbox))
        ("gem_hrdps_continental"
         (:name . "GEM HRDPS Continental")
         (:type . checkbox))
        ("meteofrance_seamless"
         (:name . "Météo-France Seamless")
         (:type . checkbox))
        ("meteofrance_arpege_world"
         (:name . "Météo-France ARPEGE World")
         (:type . checkbox))
        ("meteofrance_arpege_europe"
         (:name . "Météo-France ARPEGE Europe")
         (:type . checkbox))
        ("meteofrance_arome_france"
         (:name . "Météo-France AROME France")
         (:type . checkbox))
        ("meteofrance_arome_france_hd"
         (:name . "Météo-France AROME France HD")
         (:type . checkbox))
        ("arpae_cosmo_seamless"
         (:name . "ARPAE Seamless")
         (:type . checkbox))
        ("arpae_cosmo_2i"
         (:name . "ARPAE COSMO 2I")
         (:type . checkbox))
        ("arpae_cosmo_2i_ruc"
         (:name . "ARPAE COSMO 2I RUC")
         (:type . checkbox))
        ("arpae_cosmo_5m"
         (:name . "ARPAE COSMO 5M")
         (:type . checkbox))))))
    ("DWD ICON (Germany)"
     (:name . "DWD ICON (Germany)")
     (:url . "https://open-meteo.com/en/docs/dwd-api")
     (:description . "German Weather Service ICON model. 15-minutely data for Central Europe")
     (:key . "wd")
     (:sections
      ((:name . "Select Coordinates and Time")
       (:fields
        ("end_date"
         (:name . "End date")
         (:type . date))
        ("start_date"
         (:name . "Start date")
         (:type . date))
        ("forecast_days"
         (:name . "Forecast days")
         (:type . number)
         (:min . 0)
         (:max . 16))
        ("past_days"
         (:name . "Past days")
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))
        ("timezone"
         (:name . "Timezone")
         (:type . timezone))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Additional Variables And Options")
         (:fields
          ("is_day"
           (:name . "Is Day or Night")
           (:type . checkbox))
          ("lightning_potential"
           (:name . "Lightning Potential Index LPI (2)")
           (:type . checkbox))
          ("updraft"
           (:name . "Updraft (2)")
           (:type . checkbox))
          ("cape"
           (:name . "CAPE")
           (:type . checkbox))
          ("freezing_level_height"
           (:name . "Freezing Level Height")
           (:type . checkbox))
          ("snowfall_height"
           (:name . "Snowfall Height (1)")
           (:type . checkbox))
          ("sunshine_duration"
           (:name . "Sunshine Duration")
           (:type . checkbox))
          ("forecast_hours"
           (:name . "Forecast Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))
          ("past_hours"
           (:name . "Past Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))))
        ((:name . "Solar Radiation Variables")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation GHI")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation DHI")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("global_tilted_irradiance"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation"
           (:name . "Terrestrial Solar Radiation")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation GHI (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation DHI (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox))
          ("global_tilted_irradiance_instant"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation_instant"
           (:name . "Terrestrial Solar Radiation (Instant)")
           (:type . checkbox))
          ("tilt"
           (:name . "Panel Tilt (0° horizontal)")
           (:type . number))
          ("azimuth"
           (:name . "Panel Azimuth (0° S, -90° E, 90° W)")
           (:type . number))))
        ((:name . "Pressure Level Variables")
         (:children
          ((:name . "Temperature")
           (:fields
            ("temperature_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("temperature_975hPa"
             (:name . "975 hPa (320 m)")
             (:type . checkbox))
            ("temperature_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("temperature_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("temperature_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("temperature_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("temperature_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("temperature_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("temperature_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("temperature_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("temperature_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("temperature_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("temperature_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("temperature_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("temperature_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("temperature_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("temperature_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("temperature_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("temperature_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))))
          ((:name . "Relative Humidity")
           (:fields
            ("relative_humidity_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("relative_humidity_975hPa"
             (:name . "975 hPa (320 m)")
             (:type . checkbox))
            ("relative_humidity_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("relative_humidity_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("relative_humidity_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("relative_humidity_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("relative_humidity_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("relative_humidity_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("relative_humidity_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("relative_humidity_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("relative_humidity_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("relative_humidity_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("relative_humidity_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("relative_humidity_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("relative_humidity_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("relative_humidity_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("relative_humidity_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("relative_humidity_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("relative_humidity_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))))
          ((:name . "Cloud cover")
           (:fields
            ("cloud_cover_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("cloud_cover_975hPa"
             (:name . "975 hPa (320 m)")
             (:type . checkbox))
            ("cloud_cover_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("cloud_cover_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("cloud_cover_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("cloud_cover_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("cloud_cover_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("cloud_cover_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("cloud_cover_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("cloud_cover_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("cloud_cover_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("cloud_cover_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("cloud_cover_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("cloud_cover_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("cloud_cover_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("cloud_cover_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("cloud_cover_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("cloud_cover_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("cloud_cover_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))))
          ((:name . "Wind Speed")
           (:fields
            ("windspeed_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("windspeed_975hPa"
             (:name . "975 hPa (320 m)")
             (:type . checkbox))
            ("windspeed_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("windspeed_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("windspeed_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("windspeed_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("windspeed_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("windspeed_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("windspeed_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("windspeed_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("windspeed_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("windspeed_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("windspeed_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("windspeed_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("windspeed_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("windspeed_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("windspeed_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("windspeed_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("windspeed_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))))
          ((:name . "Wind Direction")
           (:fields
            ("winddirection_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("winddirection_975hPa"
             (:name . "975 hPa (320 m)")
             (:type . checkbox))
            ("winddirection_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("winddirection_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("winddirection_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("winddirection_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("winddirection_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("winddirection_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("winddirection_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("winddirection_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("winddirection_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("winddirection_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("winddirection_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("winddirection_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("winddirection_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("winddirection_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("winddirection_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("winddirection_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("winddirection_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))))
          ((:name . "Geopotential Height")
           (:fields
            ("geopotential_height_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("geopotential_height_975hPa"
             (:name . "975 hPa (320 m)")
             (:type . checkbox))
            ("geopotential_height_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("geopotential_height_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("geopotential_height_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("geopotential_height_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("geopotential_height_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("geopotential_height_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("geopotential_height_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("geopotential_height_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("geopotential_height_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("geopotential_height_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("geopotential_height_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("geopotential_height_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("geopotential_height_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("geopotential_height_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("geopotential_height_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("geopotential_height_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("geopotential_height_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox)))))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dew_point_2m"
         (:name . "Dewpoint (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation (rain + showers + snow)")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("showers"
         (:name . "Showers")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("snow_depth"
         (:name . "Snow Depth")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloud_cover"
         (:name . "Cloud cover Total")
         (:type . checkbox))
        ("cloud_cover_low"
         (:name . "Cloud cover Low")
         (:type . checkbox))
        ("cloud_cover_mid"
         (:name . "Cloud cover Mid")
         (:type . checkbox))
        ("cloud_cover_high"
         (:name . "Cloud cover High")
         (:type . checkbox))
        ("evapotranspiration"
         (:name . "Evapotranspiration")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))
        ("vapour_pressure_deficit"
         (:name . "Vapour Pressure Deficit")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_speed_80m"
         (:name . "Wind Speed (80 m)")
         (:type . checkbox))
        ("wind_speed_120m"
         (:name . "Wind Speed (120 m)")
         (:type . checkbox))
        ("wind_speed_180m"
         (:name . "Wind Speed (180 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_direction_80m"
         (:name . "Wind Direction (80 m)")
         (:type . checkbox))
        ("wind_direction_120m"
         (:name . "Wind Direction (120 m)")
         (:type . checkbox))
        ("wind_direction_180m"
         (:name . "Wind Direction (180 m)")
         (:type . checkbox))
        ("wind_gusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))
        ("temperature_80m"
         (:name . "Temperature (80 m)")
         (:type . checkbox))
        ("temperature_120m"
         (:name . "Temperature (120 m)")
         (:type . checkbox))
        ("temperature_180m"
         (:name . "Temperature (180 m)")
         (:type . checkbox))
        ("soil_temperature_0cm"
         (:name . "Soil Temperature (0 cm)")
         (:type . checkbox))
        ("soil_temperature_6cm"
         (:name . "Soil Temperature (6 cm)")
         (:type . checkbox))
        ("soil_temperature_18cm"
         (:name . "Soil Temperature (18 cm)")
         (:type . checkbox))
        ("soil_temperature_54cm"
         (:name . "Soil Temperature (54 cm)")
         (:type . checkbox))
        ("soil_moisture_0_to_1cm"
         (:name . "Soil Moisture (0-1 cm)")
         (:type . checkbox))
        ("soil_moisture_1_to_3cm"
         (:name . "Soil Moisture (1-3 cm)")
         (:type . checkbox))
        ("soil_moisture_3_to_9cm"
         (:name . "Soil Moisture (3-9 cm)")
         (:type . checkbox))
        ("soil_moisture_9_to_27cm"
         (:name . "Soil Moisture (9-27 cm)")
         (:type . checkbox))
        ("soil_moisture_27_to_81cm"
         (:name . "Soil Moisture (27-81 cm)")
         (:type . checkbox))))
      ((:param . "daily")
       (:name . "Daily Weather Variables")
       (:fields
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("temperature_2m_max"
         (:name . "Maximum Temperature (2 m)")
         (:type . checkbox))
        ("temperature_2m_min"
         (:name . "Minimum Temperature (2 m)")
         (:type . checkbox))
        ("apparent_temperature_max"
         (:name . "Maximum Apparent Temperature (2 m)")
         (:type . checkbox))
        ("apparent_temperature_min"
         (:name . "Minimum Apparent Temperature (2 m)")
         (:type . checkbox))
        ("sunrise"
         (:name . "Sunrise")
         (:type . checkbox))
        ("sunset"
         (:name . "Sunset")
         (:type . checkbox))
        ("daylight_duration"
         (:name . "Daylight Duration")
         (:type . checkbox))
        ("sunshine_duration"
         (:name . "Sunshine Duration")
         (:type . checkbox))
        ("precipitation_sum"
         (:name . "Precipitation Sum")
         (:type . checkbox))
        ("rain_sum"
         (:name . "Rain Sum")
         (:type . checkbox))
        ("showers_sum"
         (:name . "Showers Sum")
         (:type . checkbox))
        ("snowfall_sum"
         (:name . "Snowfall Sum")
         (:type . checkbox))
        ("precipitation_hours"
         (:name . "Precipitation Hours")
         (:type . checkbox))
        ("precipitation_probability_max"
         (:name . "Precipitation Probability Max")
         (:type . checkbox))
        ("wind_speed_10m_max"
         (:name . "Maximum Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_gusts_10m_max"
         (:name . "Maximum Wind Gusts (10 m)")
         (:type . checkbox))
        ("wind_direction_10m_dominant"
         (:name . "Dominant Wind Direction (10 m)")
         (:type . checkbox))
        ("shortwave_radiation_sum"
         (:name . "Shortwave Radiation Sum")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))))
      ((:param . "minutely_15")
       (:name . "15-Minutely Weather Variables")
       (:fields
        ("precipitation"
         (:name . "Precipitation (rain + showers + snow)")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("snowfall_height"
         (:name . "Snowfall Height")
         (:type . checkbox))
        ("freezing_level_height"
         (:name . "Freezing Level Height")
         (:type . checkbox))
        ("cape"
         (:name . "CAPE")
         (:type . checkbox))
        ("lightning_potential"
         (:name . "Lightning Potential Index LPI")
         (:type . checkbox))
        ("is_day"
         (:name . "Is Day or Night")
         (:type . checkbox))
        ("sunshine_duration"
         (:name . "Sunshine Duration")
         (:type . checkbox))
        ("shortwave_radiation"
         (:name . "Shortwave Solar Radiation GHI")
         (:type . checkbox))
        ("direct_radiation"
         (:name . "Direct Solar Radiation")
         (:type . checkbox))
        ("diffuse_radiation"
         (:name . "Diffuse Solar Radiation DHI")
         (:type . checkbox))
        ("direct_normal_irradiance"
         (:name . "Direct Normal Irradiance DNI")
         (:type . checkbox))
        ("global_tilted_irradiance"
         (:name . "Global Tilted Radiation GTI")
         (:type . checkbox))
        ("terrestrial_radiation"
         (:name . "Terrestrial Solar Radiation")
         (:type . checkbox))
        ("shortwave_radiation_instant"
         (:name . "Shortwave Solar Radiation GHI (Instant)")
         (:type . checkbox))
        ("direct_radiation_instant"
         (:name . "Direct Solar Radiation (Instant)")
         (:type . checkbox))
        ("diffuse_radiation_instant"
         (:name . "Diffuse Solar Radiation DHI (Instant)")
         (:type . checkbox))
        ("direct_normal_irradiance_instant"
         (:name . "Direct Normal Irradiance DNI (Instant)")
         (:type . checkbox))
        ("global_tilted_irradiance_instant"
         (:name . "Global Tilted Radiation GTI")
         (:type . checkbox))
        ("terrestrial_radiation_instant"
         (:name . "Terrestrial Solar Radiation (Instant)")
         (:type . checkbox))
        ("forecast_minutely_15"
         (:name . "Forecast Minutely 15")
         (:type . select)
         (:options
          ("4" . "1 hour")
          ("24" . "6 hours")
          ("48" . "12 hours")
          ("96" . "24 hours")))
        ("past_minutely_15"
         (:name . "Past Minutely 15")
         (:type . select)
         (:options
          ("1" . "1 hour")
          ("6" . "6 hours")
          ("12" . "12 hours")
          ("24" . "24 hours")))))
      ((:param . "current")
       (:name . "Current Weather")
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("is_day"
         (:name . "Is Day or Night")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("showers"
         (:name . "Showers")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("cloud_cover"
         (:name . "Cloud cover Total")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_gusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))))
      ((:name . "Settings")
       (:fields
        ("elevation"
         (:name . "Elevation")
         (:type . float))
        ("temperature_unit"
         (:name . "Temperature Unit")
         (:type . select)
         (:options
          ("celsius" . "Celsius °C")
          ("fahrenheit" . "Fahrenheit °F")))
        ("wind_speed_unit"
         (:name . "Wind Speed Unit")
         (:type . select)
         (:options
          ("kmh" . "Km/h")
          ("ms" . "m/s")
          ("mph" . "Mph")
          ("kn" . "Knots")))
        ("precipitation_unit"
         (:name . "Precipitation Unit")
         (:type . select)
         (:options
          ("mm" . "Millimeter")
          ("inch" . "Inch")))
        ("timeformat"
         (:name . "Timeformat")
         (:type . select)
         (:options
          ("iso8601" . "ISO 8601 (e.g. 2022-12-31)")
          ("unixtime" . "Unix timestamp")))))
      ((:param . "models")
       (:name . "Weather models")
       (:fields
        ("icon_seamless"
         (:name . "DWD ICON Seamless")
         (:type . checkbox))
        ("icon_global"
         (:name . "DWD ICON Global")
         (:type . checkbox))
        ("icon_eu"
         (:name . "DWD ICON EU")
         (:type . checkbox))
        ("icon_d2"
         (:name . "DWD ICON D2")
         (:type . checkbox))))))
    ("NOAA GFS & HRRR (U.S.)"
     (:name . "NOAA GFS & HRRR (U.S.)")
     (:url . "https://open-meteo.com/en/docs/gfs-api")
     (:description . "Forecasts tailored for the US region")
     (:key . "wu")
     (:sections
      ((:name . "Select Coordinates and Time")
       (:fields
        ("end_date"
         (:name . "End date")
         (:type . date))
        ("start_date"
         (:name . "Start date")
         (:type . date))
        ("forecast_days"
         (:name . "Forecast days")
         (:type . number)
         (:min . 0)
         (:max . 16))
        ("past_days"
         (:name . "Past days")
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))
        ("timezone"
         (:name . "Timezone")
         (:type . timezone))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Additional Variables And Options")
         (:fields
          ("uv_index"
           (:name . "UV Index")
           (:type . checkbox))
          ("uv_index_clear_sky"
           (:name . "UV Index Clear Sky")
           (:type . checkbox))
          ("is_day"
           (:name . "Is Day or Night")
           (:type . checkbox))
          ("sunshine_duration"
           (:name . "Sunshine Duration")
           (:type . checkbox))
          ("cape"
           (:name . "CAPE")
           (:type . checkbox))
          ("lifted_index"
           (:name . "Lifted Index")
           (:type . checkbox))
          ("convective_inhibition"
           (:name . "Convective Inhibition")
           (:type . checkbox))
          ("freezing_level_height"
           (:name . "Freezing Level Height")
           (:type . checkbox))
          ("forecast_hours"
           (:name . "Forecast Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))
          ("past_hours"
           (:name . "Past Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))))
        ((:name . "Solar Radiation Variables")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation GHI")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation DHI")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("global_tilted_irradiance"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation"
           (:name . "Terrestrial Solar Radiation")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation GHI (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation DHI (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox))
          ("global_tilted_irradiance_instant"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation_instant"
           (:name . "Terrestrial Solar Radiation (Instant)")
           (:type . checkbox))
          ("tilt"
           (:name . "Panel Tilt (0° horizontal)")
           (:type . number))
          ("azimuth"
           (:name . "Panel Azimuth (0° S, -90° E, 90° W)")
           (:type . number))))
        ((:name . "Pressure Level Variables")
         (:children
          ((:name . "Temperature")
           (:fields
            ("temperature_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("temperature_975hPa"
             (:name . "975 hPa (320 m)")
             (:type . checkbox))
            ("temperature_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("temperature_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("temperature_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("temperature_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("temperature_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("temperature_825hPa"
             (:name . "825 hPa (1700 m)")
             (:type . checkbox))
            ("temperature_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("temperature_775hPa"
             (:name . "775 hPa (2.2 km)")
             (:type . checkbox))
            ("temperature_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("temperature_725hPa"
             (:name . "725 hPa (2.7 km)")
             (:type . checkbox))
            ("temperature_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("temperature_675hPa"
             (:name . "675 hPa (3.3 km)")
             (:type . checkbox))
            ("temperature_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("temperature_625hPa"
             (:name . "625 hPa (3.9 km)")
             (:type . checkbox))
            ("temperature_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("temperature_575hPa"
             (:name . "575 hPa (4.5 km)")
             (:type . checkbox))
            ("temperature_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("temperature_525hPa"
             (:name . "525 hPa (5.2 km)")
             (:type . checkbox))
            ("temperature_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("temperature_475hPa"
             (:name . "475 hPa (6 km)")
             (:type . checkbox))
            ("temperature_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("temperature_425hPa"
             (:name . "425 hPa (6.8 km)")
             (:type . checkbox))
            ("temperature_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("temperature_375hPa"
             (:name . "375 hPa (7.6 km)")
             (:type . checkbox))
            ("temperature_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("temperature_325hPa"
             (:name . "325 hPa (8.6 km)")
             (:type . checkbox))
            ("temperature_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("temperature_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("temperature_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("temperature_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("temperature_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("temperature_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("temperature_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("temperature_125hPa"
             (:name . "125 hPa (14.6 km)")
             (:type . checkbox))
            ("temperature_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("temperature_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("temperature_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("temperature_40hPa"
             (:name . "40 hPa (20 km)")
             (:type . checkbox))
            ("temperature_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("temperature_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("temperature_15hPa"
             (:name . "15 hPa (24 km)")
             (:type . checkbox))
            ("temperature_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))))
          ((:name . "Dewpoint")
           (:fields
            ("dew_point_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("dew_point_975hPa"
             (:name . "975 hPa (320 m)")
             (:type . checkbox))
            ("dew_point_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("dew_point_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("dew_point_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("dew_point_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("dew_point_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("dew_point_825hPa"
             (:name . "825 hPa (1700 m)")
             (:type . checkbox))
            ("dew_point_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("dew_point_775hPa"
             (:name . "775 hPa (2.2 km)")
             (:type . checkbox))
            ("dew_point_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("dew_point_725hPa"
             (:name . "725 hPa (2.7 km)")
             (:type . checkbox))
            ("dew_point_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("dew_point_675hPa"
             (:name . "675 hPa (3.3 km)")
             (:type . checkbox))
            ("dew_point_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("dew_point_625hPa"
             (:name . "625 hPa (3.9 km)")
             (:type . checkbox))
            ("dew_point_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("dew_point_575hPa"
             (:name . "575 hPa (4.5 km)")
             (:type . checkbox))
            ("dew_point_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("dew_point_525hPa"
             (:name . "525 hPa (5.2 km)")
             (:type . checkbox))
            ("dew_point_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("dew_point_475hPa"
             (:name . "475 hPa (6 km)")
             (:type . checkbox))
            ("dew_point_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("dew_point_425hPa"
             (:name . "425 hPa (6.8 km)")
             (:type . checkbox))
            ("dew_point_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("dew_point_375hPa"
             (:name . "375 hPa (7.6 km)")
             (:type . checkbox))
            ("dew_point_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("dew_point_325hPa"
             (:name . "325 hPa (8.6 km)")
             (:type . checkbox))
            ("dew_point_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("dew_point_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("dew_point_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("dew_point_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("dew_point_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("dew_point_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("dew_point_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("dew_point_125hPa"
             (:name . "125 hPa (14.6 km)")
             (:type . checkbox))
            ("dew_point_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("dew_point_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("dew_point_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("dew_point_40hPa"
             (:name . "40 hPa (20 km)")
             (:type . checkbox))
            ("dew_point_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("dew_point_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("dew_point_15hPa"
             (:name . "15 hPa (24 km)")
             (:type . checkbox))
            ("dew_point_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))))
          ((:name . "Relative Humidity")
           (:fields
            ("relative_humidity_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("relative_humidity_975hPa"
             (:name . "975 hPa (320 m)")
             (:type . checkbox))
            ("relative_humidity_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("relative_humidity_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("relative_humidity_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("relative_humidity_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("relative_humidity_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("relative_humidity_825hPa"
             (:name . "825 hPa (1700 m)")
             (:type . checkbox))
            ("relative_humidity_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("relative_humidity_775hPa"
             (:name . "775 hPa (2.2 km)")
             (:type . checkbox))
            ("relative_humidity_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("relative_humidity_725hPa"
             (:name . "725 hPa (2.7 km)")
             (:type . checkbox))
            ("relative_humidity_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("relative_humidity_675hPa"
             (:name . "675 hPa (3.3 km)")
             (:type . checkbox))
            ("relative_humidity_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("relative_humidity_625hPa"
             (:name . "625 hPa (3.9 km)")
             (:type . checkbox))
            ("relative_humidity_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("relative_humidity_575hPa"
             (:name . "575 hPa (4.5 km)")
             (:type . checkbox))
            ("relative_humidity_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("relative_humidity_525hPa"
             (:name . "525 hPa (5.2 km)")
             (:type . checkbox))
            ("relative_humidity_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("relative_humidity_475hPa"
             (:name . "475 hPa (6 km)")
             (:type . checkbox))
            ("relative_humidity_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("relative_humidity_425hPa"
             (:name . "425 hPa (6.8 km)")
             (:type . checkbox))
            ("relative_humidity_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("relative_humidity_375hPa"
             (:name . "375 hPa (7.6 km)")
             (:type . checkbox))
            ("relative_humidity_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("relative_humidity_325hPa"
             (:name . "325 hPa (8.6 km)")
             (:type . checkbox))
            ("relative_humidity_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("relative_humidity_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("relative_humidity_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("relative_humidity_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("relative_humidity_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("relative_humidity_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("relative_humidity_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("relative_humidity_125hPa"
             (:name . "125 hPa (14.6 km)")
             (:type . checkbox))
            ("relative_humidity_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("relative_humidity_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("relative_humidity_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("relative_humidity_40hPa"
             (:name . "40 hPa (20 km)")
             (:type . checkbox))
            ("relative_humidity_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("relative_humidity_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("relative_humidity_15hPa"
             (:name . "15 hPa (24 km)")
             (:type . checkbox))
            ("relative_humidity_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))))
          ((:name . "Cloud cover")
           (:fields
            ("cloud_cover_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("cloud_cover_975hPa"
             (:name . "975 hPa (320 m)")
             (:type . checkbox))
            ("cloud_cover_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("cloud_cover_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("cloud_cover_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("cloud_cover_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("cloud_cover_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("cloud_cover_825hPa"
             (:name . "825 hPa (1700 m)")
             (:type . checkbox))
            ("cloud_cover_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("cloud_cover_775hPa"
             (:name . "775 hPa (2.2 km)")
             (:type . checkbox))
            ("cloud_cover_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("cloud_cover_725hPa"
             (:name . "725 hPa (2.7 km)")
             (:type . checkbox))
            ("cloud_cover_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("cloud_cover_675hPa"
             (:name . "675 hPa (3.3 km)")
             (:type . checkbox))
            ("cloud_cover_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("cloud_cover_625hPa"
             (:name . "625 hPa (3.9 km)")
             (:type . checkbox))
            ("cloud_cover_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("cloud_cover_575hPa"
             (:name . "575 hPa (4.5 km)")
             (:type . checkbox))
            ("cloud_cover_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("cloud_cover_525hPa"
             (:name . "525 hPa (5.2 km)")
             (:type . checkbox))
            ("cloud_cover_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("cloud_cover_475hPa"
             (:name . "475 hPa (6 km)")
             (:type . checkbox))
            ("cloud_cover_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("cloud_cover_425hPa"
             (:name . "425 hPa (6.8 km)")
             (:type . checkbox))
            ("cloud_cover_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("cloud_cover_375hPa"
             (:name . "375 hPa (7.6 km)")
             (:type . checkbox))
            ("cloud_cover_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("cloud_cover_325hPa"
             (:name . "325 hPa (8.6 km)")
             (:type . checkbox))
            ("cloud_cover_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("cloud_cover_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("cloud_cover_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("cloud_cover_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("cloud_cover_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("cloud_cover_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("cloud_cover_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("cloud_cover_125hPa"
             (:name . "125 hPa (14.6 km)")
             (:type . checkbox))
            ("cloud_cover_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("cloud_cover_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("cloud_cover_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("cloud_cover_40hPa"
             (:name . "40 hPa (20 km)")
             (:type . checkbox))
            ("cloud_cover_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("cloud_cover_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("cloud_cover_15hPa"
             (:name . "15 hPa (24 km)")
             (:type . checkbox))
            ("cloud_cover_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))))
          ((:name . "Wind Speed")
           (:fields
            ("windspeed_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("windspeed_975hPa"
             (:name . "975 hPa (320 m)")
             (:type . checkbox))
            ("windspeed_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("windspeed_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("windspeed_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("windspeed_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("windspeed_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("windspeed_825hPa"
             (:name . "825 hPa (1700 m)")
             (:type . checkbox))
            ("windspeed_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("windspeed_775hPa"
             (:name . "775 hPa (2.2 km)")
             (:type . checkbox))
            ("windspeed_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("windspeed_725hPa"
             (:name . "725 hPa (2.7 km)")
             (:type . checkbox))
            ("windspeed_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("windspeed_675hPa"
             (:name . "675 hPa (3.3 km)")
             (:type . checkbox))
            ("windspeed_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("windspeed_625hPa"
             (:name . "625 hPa (3.9 km)")
             (:type . checkbox))
            ("windspeed_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("windspeed_575hPa"
             (:name . "575 hPa (4.5 km)")
             (:type . checkbox))
            ("windspeed_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("windspeed_525hPa"
             (:name . "525 hPa (5.2 km)")
             (:type . checkbox))
            ("windspeed_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("windspeed_475hPa"
             (:name . "475 hPa (6 km)")
             (:type . checkbox))
            ("windspeed_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("windspeed_425hPa"
             (:name . "425 hPa (6.8 km)")
             (:type . checkbox))
            ("windspeed_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("windspeed_375hPa"
             (:name . "375 hPa (7.6 km)")
             (:type . checkbox))
            ("windspeed_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("windspeed_325hPa"
             (:name . "325 hPa (8.6 km)")
             (:type . checkbox))
            ("windspeed_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("windspeed_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("windspeed_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("windspeed_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("windspeed_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("windspeed_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("windspeed_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("windspeed_125hPa"
             (:name . "125 hPa (14.6 km)")
             (:type . checkbox))
            ("windspeed_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("windspeed_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("windspeed_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("windspeed_40hPa"
             (:name . "40 hPa (20 km)")
             (:type . checkbox))
            ("windspeed_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("windspeed_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("windspeed_15hPa"
             (:name . "15 hPa (24 km)")
             (:type . checkbox))
            ("windspeed_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))))
          ((:name . "Wind Direction")
           (:fields
            ("winddirection_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("winddirection_975hPa"
             (:name . "975 hPa (320 m)")
             (:type . checkbox))
            ("winddirection_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("winddirection_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("winddirection_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("winddirection_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("winddirection_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("winddirection_825hPa"
             (:name . "825 hPa (1700 m)")
             (:type . checkbox))
            ("winddirection_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("winddirection_775hPa"
             (:name . "775 hPa (2.2 km)")
             (:type . checkbox))
            ("winddirection_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("winddirection_725hPa"
             (:name . "725 hPa (2.7 km)")
             (:type . checkbox))
            ("winddirection_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("winddirection_675hPa"
             (:name . "675 hPa (3.3 km)")
             (:type . checkbox))
            ("winddirection_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("winddirection_625hPa"
             (:name . "625 hPa (3.9 km)")
             (:type . checkbox))
            ("winddirection_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("winddirection_575hPa"
             (:name . "575 hPa (4.5 km)")
             (:type . checkbox))
            ("winddirection_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("winddirection_525hPa"
             (:name . "525 hPa (5.2 km)")
             (:type . checkbox))
            ("winddirection_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("winddirection_475hPa"
             (:name . "475 hPa (6 km)")
             (:type . checkbox))
            ("winddirection_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("winddirection_425hPa"
             (:name . "425 hPa (6.8 km)")
             (:type . checkbox))
            ("winddirection_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("winddirection_375hPa"
             (:name . "375 hPa (7.6 km)")
             (:type . checkbox))
            ("winddirection_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("winddirection_325hPa"
             (:name . "325 hPa (8.6 km)")
             (:type . checkbox))
            ("winddirection_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("winddirection_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("winddirection_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("winddirection_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("winddirection_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("winddirection_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("winddirection_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("winddirection_125hPa"
             (:name . "125 hPa (14.6 km)")
             (:type . checkbox))
            ("winddirection_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("winddirection_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("winddirection_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("winddirection_40hPa"
             (:name . "40 hPa (20 km)")
             (:type . checkbox))
            ("winddirection_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("winddirection_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("winddirection_15hPa"
             (:name . "15 hPa (24 km)")
             (:type . checkbox))
            ("winddirection_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))))
          ((:name . "Vertical Velocity")
           (:fields
            ("vertical_velocity_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("vertical_velocity_975hPa"
             (:name . "975 hPa (320 m)")
             (:type . checkbox))
            ("vertical_velocity_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("vertical_velocity_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("vertical_velocity_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("vertical_velocity_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("vertical_velocity_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("vertical_velocity_825hPa"
             (:name . "825 hPa (1700 m)")
             (:type . checkbox))
            ("vertical_velocity_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("vertical_velocity_775hPa"
             (:name . "775 hPa (2.2 km)")
             (:type . checkbox))
            ("vertical_velocity_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("vertical_velocity_725hPa"
             (:name . "725 hPa (2.7 km)")
             (:type . checkbox))
            ("vertical_velocity_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("vertical_velocity_675hPa"
             (:name . "675 hPa (3.3 km)")
             (:type . checkbox))
            ("vertical_velocity_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("vertical_velocity_625hPa"
             (:name . "625 hPa (3.9 km)")
             (:type . checkbox))
            ("vertical_velocity_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("vertical_velocity_575hPa"
             (:name . "575 hPa (4.5 km)")
             (:type . checkbox))
            ("vertical_velocity_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("vertical_velocity_525hPa"
             (:name . "525 hPa (5.2 km)")
             (:type . checkbox))
            ("vertical_velocity_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("vertical_velocity_475hPa"
             (:name . "475 hPa (6 km)")
             (:type . checkbox))
            ("vertical_velocity_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("vertical_velocity_425hPa"
             (:name . "425 hPa (6.8 km)")
             (:type . checkbox))
            ("vertical_velocity_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("vertical_velocity_375hPa"
             (:name . "375 hPa (7.6 km)")
             (:type . checkbox))
            ("vertical_velocity_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("vertical_velocity_325hPa"
             (:name . "325 hPa (8.6 km)")
             (:type . checkbox))
            ("vertical_velocity_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("vertical_velocity_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("vertical_velocity_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("vertical_velocity_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("vertical_velocity_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("vertical_velocity_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("vertical_velocity_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("vertical_velocity_125hPa"
             (:name . "125 hPa (14.6 km)")
             (:type . checkbox))
            ("vertical_velocity_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("vertical_velocity_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("vertical_velocity_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("vertical_velocity_40hPa"
             (:name . "40 hPa (20 km)")
             (:type . checkbox))
            ("vertical_velocity_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("vertical_velocity_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("vertical_velocity_15hPa"
             (:name . "15 hPa (24 km)")
             (:type . checkbox))
            ("vertical_velocity_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))))
          ((:name . "Geopotential Height")
           (:fields
            ("geopotential_height_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("geopotential_height_975hPa"
             (:name . "975 hPa (320 m)")
             (:type . checkbox))
            ("geopotential_height_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("geopotential_height_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("geopotential_height_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("geopotential_height_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("geopotential_height_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("geopotential_height_825hPa"
             (:name . "825 hPa (1700 m)")
             (:type . checkbox))
            ("geopotential_height_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("geopotential_height_775hPa"
             (:name . "775 hPa (2.2 km)")
             (:type . checkbox))
            ("geopotential_height_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("geopotential_height_725hPa"
             (:name . "725 hPa (2.7 km)")
             (:type . checkbox))
            ("geopotential_height_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("geopotential_height_675hPa"
             (:name . "675 hPa (3.3 km)")
             (:type . checkbox))
            ("geopotential_height_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("geopotential_height_625hPa"
             (:name . "625 hPa (3.9 km)")
             (:type . checkbox))
            ("geopotential_height_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("geopotential_height_575hPa"
             (:name . "575 hPa (4.5 km)")
             (:type . checkbox))
            ("geopotential_height_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("geopotential_height_525hPa"
             (:name . "525 hPa (5.2 km)")
             (:type . checkbox))
            ("geopotential_height_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("geopotential_height_475hPa"
             (:name . "475 hPa (6 km)")
             (:type . checkbox))
            ("geopotential_height_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("geopotential_height_425hPa"
             (:name . "425 hPa (6.8 km)")
             (:type . checkbox))
            ("geopotential_height_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("geopotential_height_375hPa"
             (:name . "375 hPa (7.6 km)")
             (:type . checkbox))
            ("geopotential_height_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("geopotential_height_325hPa"
             (:name . "325 hPa (8.6 km)")
             (:type . checkbox))
            ("geopotential_height_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("geopotential_height_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("geopotential_height_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("geopotential_height_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("geopotential_height_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("geopotential_height_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("geopotential_height_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("geopotential_height_125hPa"
             (:name . "125 hPa (14.6 km)")
             (:type . checkbox))
            ("geopotential_height_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("geopotential_height_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("geopotential_height_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("geopotential_height_40hPa"
             (:name . "40 hPa (20 km)")
             (:type . checkbox))
            ("geopotential_height_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("geopotential_height_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("geopotential_height_15hPa"
             (:name . "15 hPa (24 km)")
             (:type . checkbox))
            ("geopotential_height_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox)))))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dew_point_2m"
         (:name . "Dewpoint (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("precipitation_probability"
         (:name . "Precipitation Probability")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation (rain + showers + snow)")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("showers"
         (:name . "Showers")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("snow_depth"
         (:name . "Snow Depth")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloud_cover"
         (:name . "Cloud cover Total")
         (:type . checkbox))
        ("cloud_cover_low"
         (:name . "Cloud cover Low")
         (:type . checkbox))
        ("cloud_cover_mid"
         (:name . "Cloud cover Mid")
         (:type . checkbox))
        ("cloud_cover_high"
         (:name . "Cloud cover High")
         (:type . checkbox))
        ("visibility"
         (:name . "Visibility")
         (:type . checkbox))
        ("evapotranspiration"
         (:name . "Evapotranspiration")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))
        ("vapour_pressure_deficit"
         (:name . "Vapour Pressure Deficit")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_speed_80m"
         (:name . "Wind Speed (80 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_direction_80m"
         (:name . "Wind Direction (80 m)")
         (:type . checkbox))
        ("wind_gusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))
        ("temperature_80m"
         (:name . "Temperature (80 m)")
         (:type . checkbox))
        ("surface_temperature"
         (:name . "Surface Temperature")
         (:type . checkbox))
        ("soil_temperature_0_to_10cm"
         (:name . "Soil Temperature (0-10 cm)")
         (:type . checkbox))
        ("soil_temperature_10_to_40cm"
         (:name . "Soil Temperature (10-40 cm)")
         (:type . checkbox))
        ("soil_temperature_40_to_100cm"
         (:name . "Soil Temperature (40-100 cm)")
         (:type . checkbox))
        ("soil_temperature_100_to_200cm"
         (:name . "Soil Temperature (100-200 cm)")
         (:type . checkbox))
        ("soil_moisture_0_to_10cm"
         (:name . "Soil Moisture (0-10 cm)")
         (:type . checkbox))
        ("soil_moisture_10_to_40cm"
         (:name . "Soil Moisture (10-40 cm)")
         (:type . checkbox))
        ("soil_moisture_40_to_100cm"
         (:name . "Soil Moisture (40-100 cm)")
         (:type . checkbox))
        ("soil_moisture_100_to_200cm"
         (:name . "Soil Moisture (100-200 cm)")
         (:type . checkbox))))
      ((:param . "daily")
       (:name . "Daily Weather Variables")
       (:fields
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("temperature_2m_max"
         (:name . "Maximum Temperature (2 m)")
         (:type . checkbox))
        ("temperature_2m_min"
         (:name . "Minimum Temperature (2 m)")
         (:type . checkbox))
        ("apparent_temperature_max"
         (:name . "Maximum Apparent Temperature (2 m)")
         (:type . checkbox))
        ("apparent_temperature_min"
         (:name . "Minimum Apparent Temperature (2 m)")
         (:type . checkbox))
        ("sunrise"
         (:name . "Sunrise")
         (:type . checkbox))
        ("sunset"
         (:name . "Sunset")
         (:type . checkbox))
        ("daylight_duration"
         (:name . "Daylight Duration")
         (:type . checkbox))
        ("sunshine_duration"
         (:name . "Sunshine Duration")
         (:type . checkbox))
        ("uv_index_max"
         (:name . "UV Index")
         (:type . checkbox))
        ("uv_index_clear_sky_max"
         (:name . "UV Index Clear Sky")
         (:type . checkbox))
        ("precipitation_sum"
         (:name . "Precipitation Sum")
         (:type . checkbox))
        ("rain_sum"
         (:name . "Rain Sum")
         (:type . checkbox))
        ("showers_sum"
         (:name . "Showers Sum")
         (:type . checkbox))
        ("snowfall_sum"
         (:name . "Snowfall Sum")
         (:type . checkbox))
        ("precipitation_hours"
         (:name . "Precipitation Hours")
         (:type . checkbox))
        ("precipitation_probability_max"
         (:name . "Precipitation Probability Max")
         (:type . checkbox))
        ("wind_speed_10m_max"
         (:name . "Maximum Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_gusts_10m_max"
         (:name . "Maximum Wind Gusts (10 m)")
         (:type . checkbox))
        ("wind_direction_10m_dominant"
         (:name . "Dominant Wind Direction (10 m)")
         (:type . checkbox))
        ("shortwave_radiation_sum"
         (:name . "Shortwave Radiation Sum")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))))
      ((:param . "minutely_15")
       (:name . "15-Minutely Weather Variables")
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dew_point_2m"
         (:name . "Dewpoint (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation (rain + showers + snow)")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("sunshine_duration"
         (:name . "Sunshine Duration")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_speed_80m"
         (:name . "Wind Speed (80 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_direction_80m"
         (:name . "Wind Direction (80 m)")
         (:type . checkbox))
        ("wind_gusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))
        ("visibility"
         (:name . "Visibility")
         (:type . checkbox))
        ("cape"
         (:name . "CAPE")
         (:type . checkbox))
        ("is_day"
         (:name . "Is Day or Night")
         (:type . checkbox))
        ("shortwave_radiation"
         (:name . "Shortwave Solar Radiation GHI")
         (:type . checkbox))
        ("direct_radiation"
         (:name . "Direct Solar Radiation")
         (:type . checkbox))
        ("diffuse_radiation"
         (:name . "Diffuse Solar Radiation DHI")
         (:type . checkbox))
        ("direct_normal_irradiance"
         (:name . "Direct Normal Irradiance DNI")
         (:type . checkbox))
        ("global_tilted_irradiance"
         (:name . "Global Tilted Radiation GTI")
         (:type . checkbox))
        ("terrestrial_radiation"
         (:name . "Terrestrial Solar Radiation")
         (:type . checkbox))
        ("shortwave_radiation_instant"
         (:name . "Shortwave Solar Radiation GHI (Instant)")
         (:type . checkbox))
        ("direct_radiation_instant"
         (:name . "Direct Solar Radiation (Instant)")
         (:type . checkbox))
        ("diffuse_radiation_instant"
         (:name . "Diffuse Solar Radiation DHI (Instant)")
         (:type . checkbox))
        ("direct_normal_irradiance_instant"
         (:name . "Direct Normal Irradiance DNI (Instant)")
         (:type . checkbox))
        ("global_tilted_irradiance_instant"
         (:name . "Global Tilted Radiation GTI")
         (:type . checkbox))
        ("terrestrial_radiation_instant"
         (:name . "Terrestrial Solar Radiation (Instant)")
         (:type . checkbox))
        ("forecast_minutely_15"
         (:name . "Forecast Minutely 15")
         (:type . select)
         (:options
          ("4" . "1 hour")
          ("24" . "6 hours")
          ("48" . "12 hours")
          ("96" . "24 hours")))
        ("past_minutely_15"
         (:name . "Past Minutely 15")
         (:type . select)
         (:options
          ("1" . "1 hour")
          ("6" . "6 hours")
          ("12" . "12 hours")
          ("24" . "24 hours")))))
      ((:param . "current")
       (:name . "Current Weather")
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("is_day"
         (:name . "Is Day or Night")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("showers"
         (:name . "Showers")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("cloud_cover"
         (:name . "Cloud cover Total")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_gusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))))
      ((:name . "Settings")
       (:fields
        ("elevation"
         (:name . "Elevation")
         (:type . float))
        ("temperature_unit"
         (:name . "Temperature Unit")
         (:type . select)
         (:options
          ("celsius" . "Celsius °C")
          ("fahrenheit" . "Fahrenheit °F")))
        ("wind_speed_unit"
         (:name . "Wind Speed Unit")
         (:type . select)
         (:options
          ("kmh" . "Km/h")
          ("ms" . "m/s")
          ("mph" . "Mph")
          ("kn" . "Knots")))
        ("precipitation_unit"
         (:name . "Precipitation Unit")
         (:type . select)
         (:options
          ("mm" . "Millimeter")
          ("inch" . "Inch")))
        ("timeformat"
         (:name . "Timeformat")
         (:type . select)
         (:options
          ("iso8601" . "ISO 8601 (e.g. 2022-12-31)")
          ("unixtime" . "Unix timestamp")))))
      ((:param . "models")
       (:name . "Weather models")
       (:fields
        ("gfs_seamless"
         (:name . "GFS Seamless")
         (:type . checkbox))
        ("gfs_global"
         (:name . "GFS Global")
         (:type . checkbox))
        ("gfs_hrrr"
         (:name . "GFS HRRR")
         (:type . checkbox))
        ("gfs_graphcast025"
         (:name . "GFS GraphCast")
         (:type . checkbox))))))
    ("MeteoFrance"
     (:name . "MeteoFrance")
     (:url . "https://open-meteo.com/en/docs/meteofrance-api")
     (:description . "Forecasts tailored for Central Europe and France")
     (:key . "wf")
     (:sections
      ((:name . "Select Coordinates and Time")
       (:fields
        ("end_date"
         (:name . "End date")
         (:type . date))
        ("start_date"
         (:name . "Start date")
         (:type . date))
        ("forecast_days"
         (:name . "Forecast days")
         (:type . number)
         (:min . 0)
         (:max . 16))
        ("past_days"
         (:name . "Past days")
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))
        ("timezone"
         (:name . "Timezone")
         (:type . timezone))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Additional Variables And Options")
         (:fields
          ("is_day"
           (:name . "Is Day or Night")
           (:type . checkbox))
          ("sunshine_duration"
           (:name . "Sunshine Duration")
           (:type . checkbox))
          ("cape"
           (:name . "CAPE")
           (:type . checkbox))
          ("forecast_hours"
           (:name . "Forecast Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))
          ("past_hours"
           (:name . "Past Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))))
        ((:name . "Solar Radiation Variables")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation GHI")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation DHI")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("global_tilted_irradiance"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation"
           (:name . "Terrestrial Solar Radiation")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation GHI (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation DHI (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox))
          ("global_tilted_irradiance_instant"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation_instant"
           (:name . "Terrestrial Solar Radiation (Instant)")
           (:type . checkbox))
          ("tilt"
           (:name . "Panel Tilt (0° horizontal)")
           (:type . number))
          ("azimuth"
           (:name . "Panel Azimuth (0° S, -90° E, 90° W)")
           (:type . number))))
        ((:name . "Pressure Level Variables")
         (:children
          ((:name . "Temperature")
           (:fields
            ("temperature_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("temperature_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("temperature_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("temperature_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("temperature_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("temperature_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("temperature_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("temperature_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("temperature_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("temperature_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("temperature_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("temperature_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("temperature_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("temperature_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("temperature_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("temperature_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("temperature_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("temperature_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("temperature_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("temperature_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("temperature_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("temperature_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("temperature_125hPa"
             (:name . "125 hPa (14.6 km)")
             (:type . checkbox))
            ("temperature_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("temperature_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("temperature_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("temperature_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("temperature_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("temperature_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))))
          ((:name . "Dewpoint")
           (:fields
            ("dew_point_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("dew_point_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("dew_point_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("dew_point_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("dew_point_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("dew_point_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("dew_point_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("dew_point_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("dew_point_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("dew_point_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("dew_point_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("dew_point_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("dew_point_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("dew_point_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("dew_point_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("dew_point_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("dew_point_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("dew_point_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("dew_point_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("dew_point_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("dew_point_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("dew_point_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("dew_point_125hPa"
             (:name . "125 hPa (14.6 km)")
             (:type . checkbox))
            ("dew_point_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("dew_point_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("dew_point_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("dew_point_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("dew_point_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("dew_point_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))))
          ((:name . "Relative Humidity")
           (:fields
            ("relative_humidity_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("relative_humidity_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("relative_humidity_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("relative_humidity_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("relative_humidity_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("relative_humidity_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("relative_humidity_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("relative_humidity_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("relative_humidity_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("relative_humidity_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("relative_humidity_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("relative_humidity_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("relative_humidity_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("relative_humidity_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("relative_humidity_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("relative_humidity_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("relative_humidity_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("relative_humidity_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("relative_humidity_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("relative_humidity_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("relative_humidity_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("relative_humidity_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("relative_humidity_125hPa"
             (:name . "125 hPa (14.6 km)")
             (:type . checkbox))
            ("relative_humidity_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("relative_humidity_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("relative_humidity_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("relative_humidity_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("relative_humidity_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("relative_humidity_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))))
          ((:name . "Cloud cover")
           (:fields
            ("cloud_cover_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("cloud_cover_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("cloud_cover_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("cloud_cover_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("cloud_cover_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("cloud_cover_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("cloud_cover_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("cloud_cover_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("cloud_cover_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("cloud_cover_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("cloud_cover_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("cloud_cover_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("cloud_cover_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("cloud_cover_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("cloud_cover_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("cloud_cover_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("cloud_cover_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("cloud_cover_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("cloud_cover_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("cloud_cover_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("cloud_cover_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("cloud_cover_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("cloud_cover_125hPa"
             (:name . "125 hPa (14.6 km)")
             (:type . checkbox))
            ("cloud_cover_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("cloud_cover_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("cloud_cover_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("cloud_cover_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("cloud_cover_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("cloud_cover_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))))
          ((:name . "Wind Speed")
           (:fields
            ("windspeed_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("windspeed_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("windspeed_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("windspeed_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("windspeed_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("windspeed_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("windspeed_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("windspeed_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("windspeed_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("windspeed_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("windspeed_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("windspeed_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("windspeed_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("windspeed_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("windspeed_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("windspeed_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("windspeed_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("windspeed_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("windspeed_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("windspeed_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("windspeed_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("windspeed_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("windspeed_125hPa"
             (:name . "125 hPa (14.6 km)")
             (:type . checkbox))
            ("windspeed_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("windspeed_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("windspeed_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("windspeed_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("windspeed_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("windspeed_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))))
          ((:name . "Wind Direction")
           (:fields
            ("winddirection_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("winddirection_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("winddirection_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("winddirection_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("winddirection_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("winddirection_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("winddirection_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("winddirection_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("winddirection_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("winddirection_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("winddirection_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("winddirection_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("winddirection_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("winddirection_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("winddirection_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("winddirection_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("winddirection_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("winddirection_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("winddirection_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("winddirection_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("winddirection_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("winddirection_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("winddirection_125hPa"
             (:name . "125 hPa (14.6 km)")
             (:type . checkbox))
            ("winddirection_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("winddirection_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("winddirection_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("winddirection_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("winddirection_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("winddirection_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))))
          ((:name . "Geopotential Height")
           (:fields
            ("geopotential_height_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("geopotential_height_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("geopotential_height_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("geopotential_height_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("geopotential_height_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("geopotential_height_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("geopotential_height_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("geopotential_height_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("geopotential_height_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("geopotential_height_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("geopotential_height_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("geopotential_height_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("geopotential_height_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("geopotential_height_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("geopotential_height_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("geopotential_height_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("geopotential_height_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("geopotential_height_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("geopotential_height_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("geopotential_height_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("geopotential_height_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("geopotential_height_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("geopotential_height_125hPa"
             (:name . "125 hPa (14.6 km)")
             (:type . checkbox))
            ("geopotential_height_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("geopotential_height_70hPa"
             (:name . "70 hPa (17.7 km)")
             (:type . checkbox))
            ("geopotential_height_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("geopotential_height_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("geopotential_height_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("geopotential_height_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox)))))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dew_point_2m"
         (:name . "Dewpoint (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation (rain + snow)")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloud_cover"
         (:name . "Cloud cover Total")
         (:type . checkbox))
        ("cloud_cover_low"
         (:name . "Cloud cover Low")
         (:type . checkbox))
        ("cloud_cover_mid"
         (:name . "Cloud cover Mid")
         (:type . checkbox))
        ("cloud_cover_high"
         (:name . "Cloud cover High")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))
        ("vapour_pressure_deficit"
         (:name . "Vapour Pressure Deficit")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_speed_20m"
         (:name . "Wind Speed (20 m)")
         (:type . checkbox))
        ("wind_speed_50m"
         (:name . "Wind Speed (50 m)")
         (:type . checkbox))
        ("wind_speed_100m"
         (:name . "Wind Speed (100 m)")
         (:type . checkbox))
        ("wind_speed_150m"
         (:name . "Wind Speed (150 m)")
         (:type . checkbox))
        ("wind_speed_200m"
         (:name . "Wind Speed (200 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_direction_20m"
         (:name . "Wind Direction (20 m)")
         (:type . checkbox))
        ("wind_direction_50m"
         (:name . "Wind Direction (50 m)")
         (:type . checkbox))
        ("wind_direction_100m"
         (:name . "Wind Direction (100 m)")
         (:type . checkbox))
        ("wind_direction_150m"
         (:name . "Wind Direction (150 m)")
         (:type . checkbox))
        ("wind_direction_200m"
         (:name . "Wind Direction (200 m)")
         (:type . checkbox))
        ("wind_gusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))
        ("temperature_20m"
         (:name . "Temperature (20 m)")
         (:type . checkbox))
        ("temperature_50m"
         (:name . "Temperature (50 m)")
         (:type . checkbox))
        ("temperature_100m"
         (:name . "Temperature (100 m)")
         (:type . checkbox))
        ("temperature_150m"
         (:name . "Temperature (150 m)")
         (:type . checkbox))
        ("temperature_200m"
         (:name . "Temperature (200 m)")
         (:type . checkbox))))
      ((:param . "daily")
       (:name . "Daily Weather Variables")
       (:fields
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("temperature_2m_max"
         (:name . "Maximum Temperature (2 m)")
         (:type . checkbox))
        ("temperature_2m_min"
         (:name . "Minimum Temperature (2 m)")
         (:type . checkbox))
        ("apparent_temperature_max"
         (:name . "Maximum Apparent Temperature (2 m)")
         (:type . checkbox))
        ("apparent_temperature_min"
         (:name . "Minimum Apparent Temperature (2 m)")
         (:type . checkbox))
        ("sunrise"
         (:name . "Sunrise")
         (:type . checkbox))
        ("sunset"
         (:name . "Sunset")
         (:type . checkbox))
        ("daylight_duration"
         (:name . "Daylight Duration")
         (:type . checkbox))
        ("sunshine_duration"
         (:name . "Sunshine Duration")
         (:type . checkbox))
        ("uv_index_max"
         (:name . "UV Index")
         (:type . checkbox))
        ("uv_index_clear_sky_max"
         (:name . "UV Index Clear Sky")
         (:type . checkbox))
        ("precipitation_sum"
         (:name . "Precipitation Sum")
         (:type . checkbox))
        ("rain_sum"
         (:name . "Rain Sum")
         (:type . checkbox))
        ("showers_sum"
         (:name . "Showers Sum")
         (:type . checkbox))
        ("snowfall_sum"
         (:name . "Snowfall Sum")
         (:type . checkbox))
        ("precipitation_hours"
         (:name . "Precipitation Hours")
         (:type . checkbox))
        ("precipitation_probability_max"
         (:name . "Precipitation Probability Max")
         (:type . checkbox))
        ("wind_speed_10m_max"
         (:name . "Maximum Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_gusts_10m_max"
         (:name . "Maximum Wind Gusts (10 m)")
         (:type . checkbox))
        ("wind_direction_10m_dominant"
         (:name . "Dominant Wind Direction (10 m)")
         (:type . checkbox))
        ("shortwave_radiation_sum"
         (:name . "Shortwave Radiation Sum")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))))
      ((:param . "minutely_15")
       (:name . "15-Minutely Weather Variables")
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dew_point_2m"
         (:name . "Dewpoint (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation (rain + showers + snow)")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_speed_20m"
         (:name . "Wind Speed (20 m)")
         (:type . checkbox))
        ("wind_speed_50m"
         (:name . "Wind Speed (50 m)")
         (:type . checkbox))
        ("wind_speed_100m"
         (:name . "Wind Speed (100 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_direction_20m"
         (:name . "Wind Direction (20 m)")
         (:type . checkbox))
        ("wind_direction_50m"
         (:name . "Wind Direction (50 m)")
         (:type . checkbox))
        ("wind_direction_100m"
         (:name . "Wind Direction (100 m)")
         (:type . checkbox))
        ("cape"
         (:name . "CAPE")
         (:type . checkbox))
        ("is_day"
         (:name . "Is Day or Night")
         (:type . checkbox))
        ("forecast_minutely_15"
         (:name . "Forecast Minutely 15")
         (:type . select)
         (:options
          ("4" . "1 hour")
          ("24" . "6 hours")
          ("48" . "12 hours")
          ("96" . "24 hours")))
        ("past_minutely_15"
         (:name . "Past Minutely 15")
         (:type . select)
         (:options
          ("1" . "1 hour")
          ("6" . "6 hours")
          ("12" . "12 hours")
          ("24" . "24 hours")))))
      ((:param . "current")
       (:name . "Current Weather")
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("is_day"
         (:name . "Is Day or Night")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("showers"
         (:name . "Showers")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("cloud_cover"
         (:name . "Cloud cover Total")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_gusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))))
      ((:name . "Settings")
       (:fields
        ("elevation"
         (:name . "Elevation")
         (:type . float))
        ("temperature_unit"
         (:name . "Temperature Unit")
         (:type . select)
         (:options
          ("celsius" . "Celsius °C")
          ("fahrenheit" . "Fahrenheit °F")))
        ("wind_speed_unit"
         (:name . "Wind Speed Unit")
         (:type . select)
         (:options
          ("kmh" . "Km/h")
          ("ms" . "m/s")
          ("mph" . "Mph")
          ("kn" . "Knots")))
        ("precipitation_unit"
         (:name . "Precipitation Unit")
         (:type . select)
         (:options
          ("mm" . "Millimeter")
          ("inch" . "Inch")))
        ("timeformat"
         (:name . "Timeformat")
         (:type . select)
         (:options
          ("iso8601" . "ISO 8601 (e.g. 2022-12-31)")
          ("unixtime" . "Unix timestamp")))))
      ((:param . "models")
       (:name . "Weather models")
       (:fields
        ("best_match"
         (:name . "Best match")
         (:type . checkbox))
        ("arpege_seamless"
         (:name . "ARPEGE Seamless")
         (:type . checkbox))
        ("arpege_world"
         (:name . "ARPEGE World")
         (:type . checkbox))
        ("arpege_europe"
         (:name . "ARPEGE Europe")
         (:type . checkbox))
        ("arome_seamless"
         (:name . "AROME Seamless")
         (:type . checkbox))
        ("arome_france"
         (:name . "AROME France")
         (:type . checkbox))
        ("arome_france_hd"
         (:name . "AROME France HD")
         (:type . checkbox))))))
    ("ECMWF"
     (:name . "ECMWF")
     (:url . "https://open-meteo.com/en/docs/ecmwf-api")
     (:description . "Open-data forecasts by ECMWF")
     (:key . "we")
     (:sections
      ((:name . "Select Coordinates and Time")
       (:fields
        ("end_date"
         (:name . "End date")
         (:type . date))
        ("start_date"
         (:name . "Start date")
         (:type . date))
        ("forecast_days"
         (:name . "Forecast days")
         (:type . number)
         (:min . 0)
         (:max . 16))
        ("past_days"
         (:name . "Past days")
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))
        ("timezone"
         (:name . "Timezone")
         (:type . timezone))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Pressure Level Variables")
         (:children
          ((:name . "Temperature")
           (:fields
            ("temperature_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("temperature_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("temperature_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("temperature_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("temperature_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("temperature_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("temperature_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("temperature_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("temperature_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("temperature_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("temperature_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("temperature_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("temperature_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))))
          ((:name . "Relative Humidity")
           (:fields
            ("relative_humidity_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("relative_humidity_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("relative_humidity_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("relative_humidity_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("relative_humidity_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("relative_humidity_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("relative_humidity_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("relative_humidity_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("relative_humidity_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("relative_humidity_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("relative_humidity_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("relative_humidity_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("relative_humidity_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))))
          ((:name . "Cloud cover")
           (:fields
            ("cloud_cover_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("cloud_cover_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("cloud_cover_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("cloud_cover_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("cloud_cover_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("cloud_cover_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("cloud_cover_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("cloud_cover_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("cloud_cover_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("cloud_cover_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("cloud_cover_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("cloud_cover_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("cloud_cover_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))))
          ((:name . "Wind Speed")
           (:fields
            ("windspeed_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("windspeed_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("windspeed_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("windspeed_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("windspeed_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("windspeed_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("windspeed_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("windspeed_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("windspeed_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("windspeed_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("windspeed_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("windspeed_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("windspeed_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))))
          ((:name . "Wind Direction")
           (:fields
            ("winddirection_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("winddirection_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("winddirection_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("winddirection_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("winddirection_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("winddirection_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("winddirection_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("winddirection_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("winddirection_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("winddirection_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("winddirection_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("winddirection_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("winddirection_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))))
          ((:name . "Geopotential Height")
           (:fields
            ("geopotential_height_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("geopotential_height_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("geopotential_height_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("geopotential_height_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("geopotential_height_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("geopotential_height_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("geopotential_height_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("geopotential_height_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("geopotential_height_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("geopotential_height_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("geopotential_height_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("geopotential_height_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("geopotential_height_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))))))
        ((:name . "Solar Radiation Variables")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation GHI")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation DHI")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("global_tilted_irradiance"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation"
           (:name . "Terrestrial Solar Radiation")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation GHI (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation DHI (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox))
          ("global_tilted_irradiance_instant"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation_instant"
           (:name . "Terrestrial Solar Radiation (Instant)")
           (:type . checkbox))
          ("tilt"
           (:name . "Panel Tilt (0° horizontal)")
           (:type . number))
          ("azimuth"
           (:name . "Panel Azimuth (0° S, -90° E, 90° W)")
           (:type . number)))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dew_point_2m"
         (:name . "Dewpoint (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation (rain + snow)")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloud_cover"
         (:name . "Cloud cover Total")
         (:type . checkbox))
        ("cloud_cover_low"
         (:name . "Cloud cover Low")
         (:type . checkbox))
        ("cloud_cover_mid"
         (:name . "Cloud cover Mid")
         (:type . checkbox))
        ("cloud_cover_high"
         (:name . "Cloud cover High")
         (:type . checkbox))
        ("vapour_pressure_deficit"
         (:name . "Vapour Pressure Deficit")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_speed_100m"
         (:name . "Wind Speed (100 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_direction_100m"
         (:name . "Wind Direction (100 m)")
         (:type . checkbox))
        ("surface_temperature"
         (:name . "Surface temperature")
         (:type . checkbox))
        ("soil_temperature_0_to_7cm"
         (:name . "Soil Temperature (0-7 cm)")
         (:type . checkbox))
        ("soil_moisture_0_to_7cm"
         (:name . "Soil Moisture (0-7 cm)")
         (:type . checkbox))
        ("soil_moisture_7_to_28cm"
         (:name . "Soil Moisture (7-28 cm)")
         (:type . checkbox))
        ("runoff"
         (:name . "Surface Water Runoff")
         (:type . checkbox))
        ("cape"
         (:name . "CAPE")
         (:type . checkbox))
        ("total_column_integrated_water_vapour"
         (:name . "Total Column Integrated Water Vapour")
         (:type . checkbox))))
      ((:name . "Settings")
       (:fields
        ("elevation"
         (:name . "Elevation")
         (:type . float))
        ("temperature_unit"
         (:name . "Temperature Unit")
         (:type . select)
         (:options
          ("celsius" . "Celsius °C")
          ("fahrenheit" . "Fahrenheit °F")))
        ("wind_speed_unit"
         (:name . "Wind Speed Unit")
         (:type . select)
         (:options
          ("kmh" . "Km/h")
          ("ms" . "m/s")
          ("mph" . "Mph")
          ("kn" . "Knots")))
        ("precipitation_unit"
         (:name . "Precipitation Unit")
         (:type . select)
         (:options
          ("mm" . "Millimeter")
          ("inch" . "Inch")))
        ("timeformat"
         (:name . "Timeformat")
         (:type . select)
         (:options
          ("iso8601" . "ISO 8601 (e.g. 2022-12-31)")
          ("unixtime" . "Unix timestamp")))))
      ((:param . "models")
       (:name . "Weather models")
       (:fields
        ("ecmwf_ifs04"
         (:name . "ECMWF IFS 0.4°")
         (:type . checkbox))
        ("ecmwf_ifs025"
         (:name . "ECMWF IFS 0.25°")
         (:type . checkbox))
        ("ecmwf_aifs025"
         (:name . "ECMWF AIFS 0.25°")
         (:type . checkbox))))))
    ("JMA (Japan)"
     (:name . "JMA (Japan)")
     (:url . "https://open-meteo.com/en/docs/jma-api")
     (:description . "Forecasts tailored for Japan")
     (:key . "wj")
     (:sections
      ((:name . "Select Coordinates and Time")
       (:fields
        ("end_date"
         (:name . "End date")
         (:type . date))
        ("start_date"
         (:name . "Start date")
         (:type . date))
        ("forecast_days"
         (:name . "Forecast days")
         (:type . number)
         (:min . 0)
         (:max . 16))
        ("past_days"
         (:name . "Past days")
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))
        ("timezone"
         (:name . "Timezone")
         (:type . timezone))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Additional Variables And Options")
         (:fields
          ("is_day"
           (:name . "Is Day or Night")
           (:type . checkbox))
          ("sunshine_duration"
           (:name . "Sunshine Duration")
           (:type . checkbox))
          ("forecast_hours"
           (:name . "Forecast Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))
          ("past_hours"
           (:name . "Past Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))))
        ((:name . "Solar Radiation Variables (Only available for Japan)")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation GHI")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation DHI")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("global_tilted_irradiance"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation"
           (:name . "Terrestrial Solar Radiation")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation GHI (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation DHI (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox))
          ("global_tilted_irradiance_instant"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation_instant"
           (:name . "Terrestrial Solar Radiation (Instant)")
           (:type . checkbox))
          ("tilt"
           (:name . "Panel Tilt (0° horizontal)")
           (:type . number))
          ("azimuth"
           (:name . "Panel Azimuth (0° S, -90° E, 90° W)")
           (:type . number))))
        ((:name . "Pressure Level Variables")
         (:children
          ((:name . "Temperature")
           (:fields
            ("temperature_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("temperature_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("temperature_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("temperature_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("temperature_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("temperature_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("temperature_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("temperature_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("temperature_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("temperature_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("temperature_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))))
          ((:name . "Dewpoint")
           (:fields
            ("dew_point_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("dew_point_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("dew_point_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("dew_point_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("dew_point_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("dew_point_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("dew_point_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("dew_point_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("dew_point_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("dew_point_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("dew_point_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))))
          ((:name . "Relative Humidity")
           (:fields
            ("relative_humidity_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("relative_humidity_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("relative_humidity_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("relative_humidity_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("relative_humidity_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("relative_humidity_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("relative_humidity_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("relative_humidity_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("relative_humidity_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("relative_humidity_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("relative_humidity_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))))
          ((:name . "Cloud cover")
           (:fields
            ("cloud_cover_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("cloud_cover_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("cloud_cover_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("cloud_cover_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("cloud_cover_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("cloud_cover_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("cloud_cover_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("cloud_cover_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("cloud_cover_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("cloud_cover_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("cloud_cover_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))))
          ((:name . "Wind Speed")
           (:fields
            ("windspeed_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("windspeed_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("windspeed_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("windspeed_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("windspeed_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("windspeed_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("windspeed_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("windspeed_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("windspeed_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("windspeed_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("windspeed_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))))
          ((:name . "Wind Direction")
           (:fields
            ("winddirection_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("winddirection_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("winddirection_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("winddirection_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("winddirection_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("winddirection_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("winddirection_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("winddirection_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("winddirection_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("winddirection_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("winddirection_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))))
          ((:name . "Geopotential Height")
           (:fields
            ("geopotential_height_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("geopotential_height_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("geopotential_height_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("geopotential_height_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("geopotential_height_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("geopotential_height_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("geopotential_height_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("geopotential_height_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("geopotential_height_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("geopotential_height_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("geopotential_height_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox)))))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dew_point_2m"
         (:name . "Dewpoint (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation (rain + showers + snow)")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloud_cover"
         (:name . "Cloud cover Total")
         (:type . checkbox))
        ("cloud_cover_low"
         (:name . "Cloud cover Low")
         (:type . checkbox))
        ("cloud_cover_mid"
         (:name . "Cloud cover Mid")
         (:type . checkbox))
        ("cloud_cover_high"
         (:name . "Cloud cover High")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))
        ("vapour_pressure_deficit"
         (:name . "Vapour Pressure Deficit")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))))
      ((:param . "daily")
       (:name . "Daily Weather Variables")
       (:fields
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("temperature_2m_max"
         (:name . "Maximum Temperature (2 m)")
         (:type . checkbox))
        ("temperature_2m_min"
         (:name . "Minimum Temperature (2 m)")
         (:type . checkbox))
        ("apparent_temperature_max"
         (:name . "Maximum Apparent Temperature (2 m)")
         (:type . checkbox))
        ("apparent_temperature_min"
         (:name . "Minimum Apparent Temperature (2 m)")
         (:type . checkbox))
        ("sunrise"
         (:name . "Sunrise")
         (:type . checkbox))
        ("sunset"
         (:name . "Sunset")
         (:type . checkbox))
        ("daylight_duration"
         (:name . "Daylight Duration")
         (:type . checkbox))
        ("sunshine_duration"
         (:name . "Sunshine Duration")
         (:type . checkbox))
        ("precipitation_sum"
         (:name . "Precipitation Sum")
         (:type . checkbox))
        ("rain_sum"
         (:name . "Rain Sum")
         (:type . checkbox))
        ("snowfall_sum"
         (:name . "Snowfall Sum")
         (:type . checkbox))
        ("precipitation_hours"
         (:name . "Precipitation Hours")
         (:type . checkbox))
        ("wind_speed_10m_max"
         (:name . "Maximum Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_gusts_10m_max"
         (:name . "Maximum Wind Gusts (10 m)")
         (:type . checkbox))
        ("wind_direction_10m_dominant"
         (:name . "Dominant Wind Direction (10 m)")
         (:type . checkbox))
        ("shortwave_radiation_sum"
         (:name . "Shortwave Radiation Sum")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))))
      ((:param . "current")
       (:name . "Current Weather")
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("is_day"
         (:name . "Is Day or Night")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("showers"
         (:name . "Showers")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("cloud_cover"
         (:name . "Cloud cover Total")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_gusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))))
      ((:name . "Settings")
       (:fields
        ("elevation"
         (:name . "Elevation")
         (:type . float))
        ("temperature_unit"
         (:name . "Temperature Unit")
         (:type . select)
         (:options
          ("celsius" . "Celsius °C")
          ("fahrenheit" . "Fahrenheit °F")))
        ("wind_speed_unit"
         (:name . "Wind Speed Unit")
         (:type . select)
         (:options
          ("kmh" . "Km/h")
          ("ms" . "m/s")
          ("mph" . "Mph")
          ("kn" . "Knots")))
        ("precipitation_unit"
         (:name . "Precipitation Unit")
         (:type . select)
         (:options
          ("mm" . "Millimeter")
          ("inch" . "Inch")))
        ("timeformat"
         (:name . "Timeformat")
         (:type . select)
         (:options
          ("iso8601" . "ISO 8601 (e.g. 2022-12-31)")
          ("unixtime" . "Unix timestamp")))))))
    ("MET (Norway)"
     (:name . "MET (Norway)")
     (:url . "https://open-meteo.com/en/docs/metno-api")
     (:description . "Forecasts exclusively for North Europe")
     (:key . "wn")
     (:sections
      ((:name . "Select Coordinates and Time")
       (:fields
        ("end_date"
         (:name . "End date")
         (:type . date))
        ("start_date"
         (:name . "Start date")
         (:type . date))
        ("forecast_days"
         (:name . "Forecast days")
         (:type . number)
         (:min . 0)
         (:max . 16))
        ("past_days"
         (:name . "Past days")
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))
        ("timezone"
         (:name . "Timezone")
         (:type . timezone))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Additional Variables And Options")
         (:fields
          ("is_day"
           (:name . "Is Day or Night")
           (:type . checkbox))
          ("sunshine_duration"
           (:name . "Sunshine Duration")
           (:type . checkbox))
          ("forecast_hours"
           (:name . "Forecast Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))
          ("past_hours"
           (:name . "Past Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))))
        ((:name . "Solar Radiation Variables")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation GHI")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation DHI")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("global_tilted_irradiance"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation"
           (:name . "Terrestrial Solar Radiation")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation GHI (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation DHI (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox))
          ("global_tilted_irradiance_instant"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation_instant"
           (:name . "Terrestrial Solar Radiation (Instant)")
           (:type . checkbox))
          ("tilt"
           (:name . "Panel Tilt (0° horizontal)")
           (:type . number))
          ("azimuth"
           (:name . "Panel Azimuth (0° S, -90° E, 90° W)")
           (:type . number)))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dew_point_2m"
         (:name . "Dewpoint (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation (rain + snow)")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloud_cover"
         (:name . "Cloud cover Total")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))
        ("vapour_pressure_deficit"
         (:name . "Vapour Pressure Deficit")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_gusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))))
      ((:param . "current")
       (:name . "Current Weather")
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("is_day"
         (:name . "Is Day or Night")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("showers"
         (:name . "Showers")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("cloud_cover"
         (:name . "Cloud cover Total")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_gusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))))
      ((:name . "Settings")
       (:fields
        ("elevation"
         (:name . "Elevation")
         (:type . float))
        ("temperature_unit"
         (:name . "Temperature Unit")
         (:type . select)
         (:options
          ("celsius" . "Celsius °C")
          ("fahrenheit" . "Fahrenheit °F")))
        ("wind_speed_unit"
         (:name . "Wind Speed Unit")
         (:type . select)
         (:options
          ("kmh" . "Km/h")
          ("ms" . "m/s")
          ("mph" . "Mph")
          ("kn" . "Knots")))
        ("precipitation_unit"
         (:name . "Precipitation Unit")
         (:type . select)
         (:options
          ("mm" . "Millimeter")
          ("inch" . "Inch")))
        ("timeformat"
         (:name . "Timeformat")
         (:type . select)
         (:options
          ("iso8601" . "ISO 8601 (e.g. 2022-12-31)")
          ("unixtime" . "Unix timestamp")))))))
    ("GEM (Canada)"
     (:name . "GEM (Canada)")
     (:url . "https://open-meteo.com/en/docs/gem-api")
     (:description . "Forecasts tailored for North America")
     (:key . "wa")
     (:sections
      ((:name . "Select Coordinates and Time")
       (:fields
        ("end_date"
         (:name . "End date")
         (:type . date))
        ("start_date"
         (:name . "Start date")
         (:type . date))
        ("forecast_days"
         (:name . "Forecast days")
         (:type . number)
         (:min . 0)
         (:max . 16))
        ("past_days"
         (:name . "Past days")
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))
        ("timezone"
         (:name . "Timezone")
         (:type . timezone))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Additional Variables And Options")
         (:fields
          ("is_day"
           (:name . "Is Day or Night")
           (:type . checkbox))
          ("sunshine_duration"
           (:name . "Sunshine Duration")
           (:type . checkbox))
          ("cape"
           (:name . "CAPE")
           (:type . checkbox))
          ("forecast_hours"
           (:name . "Forecast Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))
          ("past_hours"
           (:name . "Past Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))))
        ((:name . "Solar Radiation Variables")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation GHI")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation DHI")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("global_tilted_irradiance"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation"
           (:name . "Terrestrial Solar Radiation")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation GHI (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation DHI (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox))
          ("global_tilted_irradiance_instant"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation_instant"
           (:name . "Terrestrial Solar Radiation (Instant)")
           (:type . checkbox))
          ("tilt"
           (:name . "Panel Tilt (0° horizontal)")
           (:type . number))
          ("azimuth"
           (:name . "Panel Azimuth (0° S, -90° E, 90° W)")
           (:type . number))))
        ((:name . "Pressure Level Variables")
         (:children
          ((:name . "Temperature")
           (:fields
            ("temperature_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))
            ("temperature_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("temperature_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("temperature_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("temperature_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("temperature_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("temperature_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("temperature_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("temperature_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("temperature_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("temperature_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("temperature_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("temperature_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("temperature_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("temperature_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("temperature_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("temperature_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("temperature_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("temperature_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("temperature_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("temperature_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("temperature_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("temperature_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("temperature_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("temperature_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("temperature_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("temperature_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("temperature_970hPa"
             (:name . "970 hPa (370 m)")
             (:type . checkbox))
            ("temperature_985hPa"
             (:name . "985 hPa (240 m)")
             (:type . checkbox))
            ("temperature_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("temperature_1015hPa"
             (:name . "1015 hPa (-10 m)")
             (:type . checkbox))))
          ((:name . "Dewpoint")
           (:fields
            ("dew_point_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))
            ("dew_point_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("dew_point_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("dew_point_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("dew_point_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("dew_point_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("dew_point_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("dew_point_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("dew_point_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("dew_point_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("dew_point_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("dew_point_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("dew_point_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("dew_point_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("dew_point_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("dew_point_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("dew_point_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("dew_point_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("dew_point_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("dew_point_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("dew_point_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("dew_point_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("dew_point_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("dew_point_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("dew_point_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("dew_point_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("dew_point_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("dew_point_970hPa"
             (:name . "970 hPa (370 m)")
             (:type . checkbox))
            ("dew_point_985hPa"
             (:name . "985 hPa (240 m)")
             (:type . checkbox))
            ("dew_point_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("dew_point_1015hPa"
             (:name . "1015 hPa (-10 m)")
             (:type . checkbox))))
          ((:name . "Relative Humidity")
           (:fields
            ("relative_humidity_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))
            ("relative_humidity_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("relative_humidity_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("relative_humidity_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("relative_humidity_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("relative_humidity_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("relative_humidity_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("relative_humidity_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("relative_humidity_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("relative_humidity_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("relative_humidity_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("relative_humidity_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("relative_humidity_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("relative_humidity_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("relative_humidity_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("relative_humidity_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("relative_humidity_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("relative_humidity_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("relative_humidity_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("relative_humidity_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("relative_humidity_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("relative_humidity_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("relative_humidity_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("relative_humidity_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("relative_humidity_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("relative_humidity_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("relative_humidity_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("relative_humidity_970hPa"
             (:name . "970 hPa (370 m)")
             (:type . checkbox))
            ("relative_humidity_985hPa"
             (:name . "985 hPa (240 m)")
             (:type . checkbox))
            ("relative_humidity_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("relative_humidity_1015hPa"
             (:name . "1015 hPa (-10 m)")
             (:type . checkbox))))
          ((:name . "Cloud cover")
           (:fields
            ("cloud_cover_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))
            ("cloud_cover_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("cloud_cover_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("cloud_cover_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("cloud_cover_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("cloud_cover_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("cloud_cover_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("cloud_cover_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("cloud_cover_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("cloud_cover_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("cloud_cover_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("cloud_cover_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("cloud_cover_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("cloud_cover_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("cloud_cover_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("cloud_cover_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("cloud_cover_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("cloud_cover_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("cloud_cover_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("cloud_cover_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("cloud_cover_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("cloud_cover_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("cloud_cover_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("cloud_cover_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("cloud_cover_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("cloud_cover_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("cloud_cover_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("cloud_cover_970hPa"
             (:name . "970 hPa (370 m)")
             (:type . checkbox))
            ("cloud_cover_985hPa"
             (:name . "985 hPa (240 m)")
             (:type . checkbox))
            ("cloud_cover_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("cloud_cover_1015hPa"
             (:name . "1015 hPa (-10 m)")
             (:type . checkbox))))
          ((:name . "Wind Speed")
           (:fields
            ("windspeed_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))
            ("windspeed_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("windspeed_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("windspeed_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("windspeed_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("windspeed_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("windspeed_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("windspeed_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("windspeed_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("windspeed_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("windspeed_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("windspeed_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("windspeed_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("windspeed_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("windspeed_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("windspeed_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("windspeed_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("windspeed_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("windspeed_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("windspeed_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("windspeed_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("windspeed_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("windspeed_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("windspeed_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("windspeed_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("windspeed_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("windspeed_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("windspeed_970hPa"
             (:name . "970 hPa (370 m)")
             (:type . checkbox))
            ("windspeed_985hPa"
             (:name . "985 hPa (240 m)")
             (:type . checkbox))
            ("windspeed_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("windspeed_1015hPa"
             (:name . "1015 hPa (-10 m)")
             (:type . checkbox))))
          ((:name . "Wind Direction")
           (:fields
            ("winddirection_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))
            ("winddirection_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("winddirection_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("winddirection_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("winddirection_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("winddirection_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("winddirection_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("winddirection_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("winddirection_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("winddirection_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("winddirection_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("winddirection_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("winddirection_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("winddirection_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("winddirection_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("winddirection_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("winddirection_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("winddirection_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("winddirection_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("winddirection_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("winddirection_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("winddirection_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("winddirection_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("winddirection_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("winddirection_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("winddirection_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("winddirection_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("winddirection_970hPa"
             (:name . "970 hPa (370 m)")
             (:type . checkbox))
            ("winddirection_985hPa"
             (:name . "985 hPa (240 m)")
             (:type . checkbox))
            ("winddirection_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("winddirection_1015hPa"
             (:name . "1015 hPa (-10 m)")
             (:type . checkbox))))
          ((:name . "Geopotential Height")
           (:fields
            ("geopotential_height_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))
            ("geopotential_height_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("geopotential_height_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("geopotential_height_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("geopotential_height_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("geopotential_height_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("geopotential_height_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("geopotential_height_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("geopotential_height_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("geopotential_height_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("geopotential_height_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("geopotential_height_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("geopotential_height_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("geopotential_height_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("geopotential_height_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("geopotential_height_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("geopotential_height_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("geopotential_height_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("geopotential_height_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("geopotential_height_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("geopotential_height_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("geopotential_height_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("geopotential_height_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("geopotential_height_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("geopotential_height_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("geopotential_height_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("geopotential_height_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("geopotential_height_970hPa"
             (:name . "970 hPa (370 m)")
             (:type . checkbox))
            ("geopotential_height_985hPa"
             (:name . "985 hPa (240 m)")
             (:type . checkbox))
            ("geopotential_height_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("geopotential_height_1015hPa"
             (:name . "1015 hPa (-10 m)")
             (:type . checkbox)))))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dew_point_2m"
         (:name . "Dewpoint (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation (rain + showers + snow)")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("showers"
         (:name . "Showers")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloud_cover"
         (:name . "Cloud cover Total")
         (:type . checkbox))
        ("cloud_cover_low"
         (:name . "Cloud cover Low")
         (:type . checkbox))
        ("cloud_cover_mid"
         (:name . "Cloud cover Mid")
         (:type . checkbox))
        ("cloud_cover_high"
         (:name . "Cloud cover High")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))
        ("vapour_pressure_deficit"
         (:name . "Vapour Pressure Deficit")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_speed_40m"
         (:name . "Wind Speed (40 m)")
         (:type . checkbox))
        ("wind_speed_80m"
         (:name . "Wind Speed (80 m)")
         (:type . checkbox))
        ("wind_speed_120m"
         (:name . "Wind Speed (120 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_direction_40m"
         (:name . "Wind Direction (40 m)")
         (:type . checkbox))
        ("wind_direction_80m"
         (:name . "Wind Direction (80 m)")
         (:type . checkbox))
        ("wind_direction_120m"
         (:name . "Wind Direction (120 m)")
         (:type . checkbox))
        ("wind_gusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))
        ("temperature_40m"
         (:name . "Temperature (40 m)")
         (:type . checkbox))
        ("temperature_80m"
         (:name . "Temperature (80 m)")
         (:type . checkbox))
        ("temperature_120m"
         (:name . "Temperature (120 m)")
         (:type . checkbox))
        ("soil_temperature_0_to_10cm"
         (:name . "Soil Temperature (0-10 cm)")
         (:type . checkbox))
        ("soil_moisture_0_to_10cm"
         (:name . "Soil Moisture (0-10 cm)")
         (:type . checkbox))))
      ((:param . "daily")
       (:name . "Daily Weather Variables")
       (:fields
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("temperature_2m_max"
         (:name . "Maximum Temperature (2 m)")
         (:type . checkbox))
        ("temperature_2m_min"
         (:name . "Minimum Temperature (2 m)")
         (:type . checkbox))
        ("apparent_temperature_max"
         (:name . "Maximum Apparent Temperature (2 m)")
         (:type . checkbox))
        ("apparent_temperature_min"
         (:name . "Minimum Apparent Temperature (2 m)")
         (:type . checkbox))
        ("sunrise"
         (:name . "Sunrise")
         (:type . checkbox))
        ("sunset"
         (:name . "Sunset")
         (:type . checkbox))
        ("daylight_duration"
         (:name . "Daylight Duration")
         (:type . checkbox))
        ("sunshine_duration"
         (:name . "Sunshine Duration")
         (:type . checkbox))
        ("precipitation_sum"
         (:name . "Precipitation Sum")
         (:type . checkbox))
        ("rain_sum"
         (:name . "Rain Sum")
         (:type . checkbox))
        ("showers_sum"
         (:name . "Showers Sum")
         (:type . checkbox))
        ("snowfall_sum"
         (:name . "Snowfall Sum")
         (:type . checkbox))
        ("precipitation_hours"
         (:name . "Precipitation Hours")
         (:type . checkbox))
        ("wind_speed_10m_max"
         (:name . "Maximum Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_gusts_10m_max"
         (:name . "Maximum Wind Gusts (10 m)")
         (:type . checkbox))
        ("wind_direction_10m_dominant"
         (:name . "Dominant Wind Direction (10 m)")
         (:type . checkbox))
        ("shortwave_radiation_sum"
         (:name . "Shortwave Radiation Sum")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))))
      ((:param . "current")
       (:name . "Current Weather")
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("is_day"
         (:name . "Is Day or Night")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("showers"
         (:name . "Showers")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("cloud_cover"
         (:name . "Cloud cover Total")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_gusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))))
      ((:name . "Settings")
       (:fields
        ("elevation"
         (:name . "Elevation")
         (:type . float))
        ("temperature_unit"
         (:name . "Temperature Unit")
         (:type . select)
         (:options
          ("celsius" . "Celsius °C")
          ("fahrenheit" . "Fahrenheit °F")))
        ("wind_speed_unit"
         (:name . "Wind Speed Unit")
         (:type . select)
         (:options
          ("kmh" . "Km/h")
          ("ms" . "m/s")
          ("mph" . "Mph")
          ("kn" . "Knots")))
        ("precipitation_unit"
         (:name . "Precipitation Unit")
         (:type . select)
         (:options
          ("mm" . "Millimeter")
          ("inch" . "Inch")))
        ("timeformat"
         (:name . "Timeformat")
         (:type . select)
         (:options
          ("iso8601" . "ISO 8601 (e.g. 2022-12-31)")
          ("unixtime" . "Unix timestamp")))))))
    ("BOM (Australia)"
     (:name . "BOM (Australia)")
     (:url . "https://open-meteo.com/en/docs/bom-api/")
     (:description . "Weather forecasts from the Australian Bureau of Meteorology")
     (:key . "wb")
     (:sections
      ((:name . "Select Coordinates and Time")
       (:fields
        ("end_date"
         (:name . "End date")
         (:type . date))
        ("start_date"
         (:name . "Start date")
         (:type . date))
        ("forecast_days"
         (:name . "Forecast days")
         (:type . number)
         (:min . 0)
         (:max . 16))
        ("past_days"
         (:name . "Past days")
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))
        ("timezone"
         (:name . "Timezone")
         (:type . timezone))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Additional Variables And Options")
         (:fields
          ("is_day"
           (:name . "Is Day or Night")
           (:type . checkbox))
          ("sunshine_duration"
           (:name . "Sunshine Duration")
           (:type . checkbox))
          ("forecast_hours"
           (:name . "Forecast Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))
          ("past_hours"
           (:name . "Past Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))))
        ((:name . "Solar Radiation Variables")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation GHI")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation DHI")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("global_tilted_irradiance"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation"
           (:name . "Terrestrial Solar Radiation")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation GHI (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation DHI (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox))
          ("global_tilted_irradiance_instant"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation_instant"
           (:name . "Terrestrial Solar Radiation (Instant)")
           (:type . checkbox))
          ("tilt"
           (:name . "Panel Tilt (0° horizontal)")
           (:type . number))
          ("azimuth"
           (:name . "Panel Azimuth (0° S, -90° E, 90° W)")
           (:type . number)))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dew_point_2m"
         (:name . "Dewpoint (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation (rain + showers + snow)")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("showers"
         (:name . "Showers")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("snow_depth"
         (:name . "Snow Depth")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloud_cover"
         (:name . "Cloud cover Total")
         (:type . checkbox))
        ("cloud_cover_low"
         (:name . "Cloud cover Low")
         (:type . checkbox))
        ("cloud_cover_mid"
         (:name . "Cloud cover Mid")
         (:type . checkbox))
        ("cloud_cover_high"
         (:name . "Cloud cover High")
         (:type . checkbox))
        ("visibility"
         (:name . "Visibility")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))
        ("vapour_pressure_deficit"
         (:name . "Vapour Pressure Deficit")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_speed_40m"
         (:name . "Wind Speed (40 m)")
         (:type . checkbox))
        ("wind_speed_80m"
         (:name . "Wind Speed (80 m)")
         (:type . checkbox))
        ("wind_speed_120m"
         (:name . "Wind Speed (120 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_direction_40m"
         (:name . "Wind Direction (40 m)")
         (:type . checkbox))
        ("wind_direction_80m"
         (:name . "Wind Direction (80 m)")
         (:type . checkbox))
        ("wind_direction_120m"
         (:name . "Wind Direction (120 m)")
         (:type . checkbox))
        ("wind_gusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))
        ("surface_temperature"
         (:name . "Surface Temperature")
         (:type . checkbox))
        ("soil_temperature_0_to_10cm"
         (:name . "Soil Temperature (0-10 cm)")
         (:type . checkbox))
        ("soil_temperature_10_to_35cm"
         (:name . "Soil Temperature (10-35 cm)")
         (:type . checkbox))
        ("soil_temperature_35_to_100cm"
         (:name . "Soil Temperature (35-100 cm)")
         (:type . checkbox))
        ("soil_temperature_100_to_300cm"
         (:name . "Soil Temperature (100-300 cm)")
         (:type . checkbox))
        ("soil_moisture_0_to_10cm"
         (:name . "Soil Moisture (0-10 cm)")
         (:type . checkbox))
        ("soil_moisture_10_to_35cm"
         (:name . "Soil Moisture (10-35 cm)")
         (:type . checkbox))
        ("soil_moisture_35_to_100cm"
         (:name . "Soil Moisture (35-100 cm)")
         (:type . checkbox))
        ("soil_moisture_100_to_300cm"
         (:name . "Soil Moisture (100-300 cm)")
         (:type . checkbox))))
      ((:param . "daily")
       (:name . "Daily Weather Variables")
       (:fields
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("temperature_2m_max"
         (:name . "Maximum Temperature (2 m)")
         (:type . checkbox))
        ("temperature_2m_min"
         (:name . "Minimum Temperature (2 m)")
         (:type . checkbox))
        ("apparent_temperature_max"
         (:name . "Maximum Apparent Temperature (2 m)")
         (:type . checkbox))
        ("apparent_temperature_min"
         (:name . "Minimum Apparent Temperature (2 m)")
         (:type . checkbox))
        ("sunrise"
         (:name . "Sunrise")
         (:type . checkbox))
        ("sunset"
         (:name . "Sunset")
         (:type . checkbox))
        ("daylight_duration"
         (:name . "Daylight Duration")
         (:type . checkbox))
        ("sunshine_duration"
         (:name . "Sunshine Duration")
         (:type . checkbox))
        ("uv_index_max"
         (:name . "UV Index")
         (:type . checkbox))
        ("uv_index_clear_sky_max"
         (:name . "UV Index Clear Sky")
         (:type . checkbox))
        ("precipitation_sum"
         (:name . "Precipitation Sum")
         (:type . checkbox))
        ("rain_sum"
         (:name . "Rain Sum")
         (:type . checkbox))
        ("showers_sum"
         (:name . "Showers Sum")
         (:type . checkbox))
        ("snowfall_sum"
         (:name . "Snowfall Sum")
         (:type . checkbox))
        ("precipitation_hours"
         (:name . "Precipitation Hours")
         (:type . checkbox))
        ("wind_speed_10m_max"
         (:name . "Maximum Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_gusts_10m_max"
         (:name . "Maximum Wind Gusts (10 m)")
         (:type . checkbox))
        ("wind_direction_10m_dominant"
         (:name . "Dominant Wind Direction (10 m)")
         (:type . checkbox))
        ("shortwave_radiation_sum"
         (:name . "Shortwave Radiation Sum")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))))
      ((:name . "Settings")
       (:fields
        ("elevation"
         (:name . "Elevation")
         (:type . float))
        ("temperature_unit"
         (:name . "Temperature Unit")
         (:type . select)
         (:options
          ("celsius" . "Celsius °C")
          ("fahrenheit" . "Fahrenheit °F")))
        ("wind_speed_unit"
         (:name . "Wind Speed Unit")
         (:type . select)
         (:options
          ("kmh" . "Km/h")
          ("ms" . "m/s")
          ("mph" . "Mph")
          ("kn" . "Knots")))
        ("precipitation_unit"
         (:name . "Precipitation Unit")
         (:type . select)
         (:options
          ("mm" . "Millimeter")
          ("inch" . "Inch")))
        ("timeformat"
         (:name . "Timeformat")
         (:type . select)
         (:options
          ("iso8601" . "ISO 8601 (e.g. 2022-12-31)")
          ("unixtime" . "Unix timestamp")))))))
    ("CMA (China)"
     (:name . "CMA (China)")
     (:url . "https://open-meteo.com/en/docs/gem-api")
     (:description . "Weather forecasts from the Chinese Meteorological Administration")
     (:key . "wc")
     (:sections
      ((:name . "Select Coordinates and Time")
       (:fields
        ("end_date"
         (:name . "End date")
         (:type . date))
        ("start_date"
         (:name . "Start date")
         (:type . date))
        ("forecast_days"
         (:name . "Forecast days")
         (:type . number)
         (:min . 0)
         (:max . 16))
        ("past_days"
         (:name . "Past days")
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))
        ("timezone"
         (:name . "Timezone")
         (:type . timezone))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Additional Variables And Options")
         (:fields
          ("is_day"
           (:name . "Is Day or Night")
           (:type . checkbox))
          ("sunshine_duration"
           (:name . "Sunshine Duration")
           (:type . checkbox))
          ("cape"
           (:name . "CAPE")
           (:type . checkbox))
          ("forecast_hours"
           (:name . "Forecast Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))
          ("past_hours"
           (:name . "Past Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))))
        ((:name . "Solar Radiation Variables")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation GHI")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation DHI")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("global_tilted_irradiance"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation"
           (:name . "Terrestrial Solar Radiation")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation GHI (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation DHI (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox))
          ("global_tilted_irradiance_instant"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation_instant"
           (:name . "Terrestrial Solar Radiation (Instant)")
           (:type . checkbox))
          ("tilt"
           (:name . "Panel Tilt (0° horizontal)")
           (:type . number))
          ("azimuth"
           (:name . "Panel Azimuth (0° S, -90° E, 90° W)")
           (:type . number))))
        ((:name . "Pressure Level Variables")
         (:children
          ((:name . "Temperature")
           (:fields
            ("temperature_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))
            ("temperature_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("temperature_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("temperature_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("temperature_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("temperature_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("temperature_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("temperature_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("temperature_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("temperature_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("temperature_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("temperature_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("temperature_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("temperature_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("temperature_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("temperature_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("temperature_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("temperature_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("temperature_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("temperature_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("temperature_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("temperature_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("temperature_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("temperature_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("temperature_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("temperature_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("temperature_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("temperature_970hPa"
             (:name . "970 hPa (370 m)")
             (:type . checkbox))
            ("temperature_985hPa"
             (:name . "985 hPa (240 m)")
             (:type . checkbox))
            ("temperature_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("temperature_1015hPa"
             (:name . "1015 hPa (-10 m)")
             (:type . checkbox))))
          ((:name . "Dewpoint")
           (:fields
            ("dew_point_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))
            ("dew_point_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("dew_point_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("dew_point_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("dew_point_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("dew_point_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("dew_point_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("dew_point_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("dew_point_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("dew_point_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("dew_point_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("dew_point_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("dew_point_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("dew_point_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("dew_point_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("dew_point_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("dew_point_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("dew_point_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("dew_point_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("dew_point_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("dew_point_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("dew_point_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("dew_point_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("dew_point_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("dew_point_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("dew_point_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("dew_point_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("dew_point_970hPa"
             (:name . "970 hPa (370 m)")
             (:type . checkbox))
            ("dew_point_985hPa"
             (:name . "985 hPa (240 m)")
             (:type . checkbox))
            ("dew_point_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("dew_point_1015hPa"
             (:name . "1015 hPa (-10 m)")
             (:type . checkbox))))
          ((:name . "Relative Humidity")
           (:fields
            ("relative_humidity_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))
            ("relative_humidity_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("relative_humidity_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("relative_humidity_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("relative_humidity_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("relative_humidity_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("relative_humidity_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("relative_humidity_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("relative_humidity_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("relative_humidity_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("relative_humidity_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("relative_humidity_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("relative_humidity_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("relative_humidity_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("relative_humidity_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("relative_humidity_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("relative_humidity_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("relative_humidity_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("relative_humidity_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("relative_humidity_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("relative_humidity_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("relative_humidity_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("relative_humidity_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("relative_humidity_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("relative_humidity_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("relative_humidity_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("relative_humidity_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("relative_humidity_970hPa"
             (:name . "970 hPa (370 m)")
             (:type . checkbox))
            ("relative_humidity_985hPa"
             (:name . "985 hPa (240 m)")
             (:type . checkbox))
            ("relative_humidity_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("relative_humidity_1015hPa"
             (:name . "1015 hPa (-10 m)")
             (:type . checkbox))))
          ((:name . "Cloud cover")
           (:fields
            ("cloud_cover_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))
            ("cloud_cover_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("cloud_cover_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("cloud_cover_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("cloud_cover_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("cloud_cover_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("cloud_cover_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("cloud_cover_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("cloud_cover_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("cloud_cover_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("cloud_cover_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("cloud_cover_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("cloud_cover_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("cloud_cover_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("cloud_cover_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("cloud_cover_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("cloud_cover_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("cloud_cover_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("cloud_cover_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("cloud_cover_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("cloud_cover_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("cloud_cover_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("cloud_cover_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("cloud_cover_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("cloud_cover_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("cloud_cover_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("cloud_cover_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("cloud_cover_970hPa"
             (:name . "970 hPa (370 m)")
             (:type . checkbox))
            ("cloud_cover_985hPa"
             (:name . "985 hPa (240 m)")
             (:type . checkbox))
            ("cloud_cover_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("cloud_cover_1015hPa"
             (:name . "1015 hPa (-10 m)")
             (:type . checkbox))))
          ((:name . "Wind Speed")
           (:fields
            ("windspeed_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))
            ("windspeed_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("windspeed_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("windspeed_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("windspeed_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("windspeed_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("windspeed_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("windspeed_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("windspeed_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("windspeed_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("windspeed_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("windspeed_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("windspeed_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("windspeed_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("windspeed_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("windspeed_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("windspeed_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("windspeed_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("windspeed_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("windspeed_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("windspeed_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("windspeed_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("windspeed_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("windspeed_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("windspeed_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("windspeed_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("windspeed_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("windspeed_970hPa"
             (:name . "970 hPa (370 m)")
             (:type . checkbox))
            ("windspeed_985hPa"
             (:name . "985 hPa (240 m)")
             (:type . checkbox))
            ("windspeed_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("windspeed_1015hPa"
             (:name . "1015 hPa (-10 m)")
             (:type . checkbox))))
          ((:name . "Wind Direction")
           (:fields
            ("winddirection_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))
            ("winddirection_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("winddirection_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("winddirection_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("winddirection_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("winddirection_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("winddirection_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("winddirection_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("winddirection_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("winddirection_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("winddirection_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("winddirection_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("winddirection_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("winddirection_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("winddirection_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("winddirection_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("winddirection_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("winddirection_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("winddirection_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("winddirection_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("winddirection_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("winddirection_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("winddirection_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("winddirection_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("winddirection_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("winddirection_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("winddirection_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("winddirection_970hPa"
             (:name . "970 hPa (370 m)")
             (:type . checkbox))
            ("winddirection_985hPa"
             (:name . "985 hPa (240 m)")
             (:type . checkbox))
            ("winddirection_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("winddirection_1015hPa"
             (:name . "1015 hPa (-10 m)")
             (:type . checkbox))))
          ((:name . "Geopotential Height")
           (:fields
            ("geopotential_height_10hPa"
             (:name . "10 hPa (26 km)")
             (:type . checkbox))
            ("geopotential_height_20hPa"
             (:name . "20 hPa (23 km)")
             (:type . checkbox))
            ("geopotential_height_30hPa"
             (:name . "30 hPa (22 km)")
             (:type . checkbox))
            ("geopotential_height_50hPa"
             (:name . "50 hPa (19.3 km)")
             (:type . checkbox))
            ("geopotential_height_100hPa"
             (:name . "100 hPa (15.8 km)")
             (:type . checkbox))
            ("geopotential_height_150hPa"
             (:name . "150 hPa (13.5 km)")
             (:type . checkbox))
            ("geopotential_height_175hPa"
             (:name . "175 hPa (12.6 km)")
             (:type . checkbox))
            ("geopotential_height_200hPa"
             (:name . "200 hPa (11.8 km)")
             (:type . checkbox))
            ("geopotential_height_225hPa"
             (:name . "225 hPa (11 km)")
             (:type . checkbox))
            ("geopotential_height_250hPa"
             (:name . "250 hPa (10.4 km)")
             (:type . checkbox))
            ("geopotential_height_275hPa"
             (:name . "275 hPa (9.7 km)")
             (:type . checkbox))
            ("geopotential_height_300hPa"
             (:name . "300 hPa (9.2 km)")
             (:type . checkbox))
            ("geopotential_height_350hPa"
             (:name . "350 hPa (8.1 km)")
             (:type . checkbox))
            ("geopotential_height_400hPa"
             (:name . "400 hPa (7.2 km)")
             (:type . checkbox))
            ("geopotential_height_450hPa"
             (:name . "450 hPa (6.3 km)")
             (:type . checkbox))
            ("geopotential_height_500hPa"
             (:name . "500 hPa (5.6 km)")
             (:type . checkbox))
            ("geopotential_height_550hPa"
             (:name . "550 hPa (4.9 km)")
             (:type . checkbox))
            ("geopotential_height_600hPa"
             (:name . "600 hPa (4.2 km)")
             (:type . checkbox))
            ("geopotential_height_650hPa"
             (:name . "650 hPa (3.6 km)")
             (:type . checkbox))
            ("geopotential_height_700hPa"
             (:name . "700 hPa (3 km)")
             (:type . checkbox))
            ("geopotential_height_750hPa"
             (:name . "750 hPa (2.5 km)")
             (:type . checkbox))
            ("geopotential_height_800hPa"
             (:name . "800 hPa (1900 m)")
             (:type . checkbox))
            ("geopotential_height_850hPa"
             (:name . "850 hPa (1500 m)")
             (:type . checkbox))
            ("geopotential_height_875hPa"
             (:name . "875 hPa (1200 m)")
             (:type . checkbox))
            ("geopotential_height_900hPa"
             (:name . "900 hPa (1000 m)")
             (:type . checkbox))
            ("geopotential_height_925hPa"
             (:name . "925 hPa (800 m)")
             (:type . checkbox))
            ("geopotential_height_950hPa"
             (:name . "950 hPa (500 m)")
             (:type . checkbox))
            ("geopotential_height_970hPa"
             (:name . "970 hPa (370 m)")
             (:type . checkbox))
            ("geopotential_height_985hPa"
             (:name . "985 hPa (240 m)")
             (:type . checkbox))
            ("geopotential_height_1000hPa"
             (:name . "1000 hPa (110 m)")
             (:type . checkbox))
            ("geopotential_height_1015hPa"
             (:name . "1015 hPa (-10 m)")
             (:type . checkbox)))))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dew_point_2m"
         (:name . "Dewpoint (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation (rain + showers + snow)")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("showers"
         (:name . "Showers")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloud_cover"
         (:name . "Cloud cover Total")
         (:type . checkbox))
        ("cloud_cover_low"
         (:name . "Cloud cover Low")
         (:type . checkbox))
        ("cloud_cover_mid"
         (:name . "Cloud cover Mid")
         (:type . checkbox))
        ("cloud_cover_high"
         (:name . "Cloud cover High")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))
        ("vapour_pressure_deficit"
         (:name . "Vapour Pressure Deficit")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_speed_40m"
         (:name . "Wind Speed (40 m)")
         (:type . checkbox))
        ("wind_speed_80m"
         (:name . "Wind Speed (80 m)")
         (:type . checkbox))
        ("wind_speed_120m"
         (:name . "Wind Speed (120 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_direction_40m"
         (:name . "Wind Direction (40 m)")
         (:type . checkbox))
        ("wind_direction_80m"
         (:name . "Wind Direction (80 m)")
         (:type . checkbox))
        ("wind_direction_120m"
         (:name . "Wind Direction (120 m)")
         (:type . checkbox))
        ("wind_gusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))
        ("temperature_40m"
         (:name . "Temperature (40 m)")
         (:type . checkbox))
        ("temperature_80m"
         (:name . "Temperature (80 m)")
         (:type . checkbox))
        ("temperature_120m"
         (:name . "Temperature (120 m)")
         (:type . checkbox))
        ("soil_temperature_0_to_10cm"
         (:name . "Soil Temperature (0-10 cm)")
         (:type . checkbox))
        ("soil_moisture_0_to_10cm"
         (:name . "Soil Moisture (0-10 cm)")
         (:type . checkbox))))
      ((:param . "daily")
       (:name . "Daily Weather Variables")
       (:fields
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("temperature_2m_max"
         (:name . "Maximum Temperature (2 m)")
         (:type . checkbox))
        ("temperature_2m_min"
         (:name . "Minimum Temperature (2 m)")
         (:type . checkbox))
        ("apparent_temperature_max"
         (:name . "Maximum Apparent Temperature (2 m)")
         (:type . checkbox))
        ("apparent_temperature_min"
         (:name . "Minimum Apparent Temperature (2 m)")
         (:type . checkbox))
        ("sunrise"
         (:name . "Sunrise")
         (:type . checkbox))
        ("sunset"
         (:name . "Sunset")
         (:type . checkbox))
        ("daylight_duration"
         (:name . "Daylight Duration")
         (:type . checkbox))
        ("sunshine_duration"
         (:name . "Sunshine Duration")
         (:type . checkbox))
        ("precipitation_sum"
         (:name . "Precipitation Sum")
         (:type . checkbox))
        ("rain_sum"
         (:name . "Rain Sum")
         (:type . checkbox))
        ("showers_sum"
         (:name . "Showers Sum")
         (:type . checkbox))
        ("snowfall_sum"
         (:name . "Snowfall Sum")
         (:type . checkbox))
        ("precipitation_hours"
         (:name . "Precipitation Hours")
         (:type . checkbox))
        ("wind_speed_10m_max"
         (:name . "Maximum Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_gusts_10m_max"
         (:name . "Maximum Wind Gusts (10 m)")
         (:type . checkbox))
        ("wind_direction_10m_dominant"
         (:name . "Dominant Wind Direction (10 m)")
         (:type . checkbox))
        ("shortwave_radiation_sum"
         (:name . "Shortwave Radiation Sum")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))))
      ((:param . "current")
       (:name . "Current Weather")
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("is_day"
         (:name . "Is Day or Night")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("showers"
         (:name . "Showers")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("cloud_cover"
         (:name . "Cloud cover Total")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_gusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))))
      ((:name . "Settings")
       (:fields
        ("elevation"
         (:name . "Elevation")
         (:type . float))
        ("temperature_unit"
         (:name . "Temperature Unit")
         (:type . select)
         (:options
          ("celsius" . "Celsius °C")
          ("fahrenheit" . "Fahrenheit °F")))
        ("wind_speed_unit"
         (:name . "Wind Speed Unit")
         (:type . select)
         (:options
          ("kmh" . "Km/h")
          ("ms" . "m/s")
          ("mph" . "Mph")
          ("kn" . "Knots")))
        ("precipitation_unit"
         (:name . "Precipitation Unit")
         (:type . select)
         (:options
          ("mm" . "Millimeter")
          ("inch" . "Inch")))
        ("timeformat"
         (:name . "Timeformat")
         (:type . select)
         (:options
          ("iso8601" . "ISO 8601 (e.g. 2022-12-31)")
          ("unixtime" . "Unix timestamp")))))))
    ("Historical Weather"
     (:name . "Historical Weather")
     (:url . "https://open-meteo.com/en/docs/historical-weather-api")
     (:description . "Weather information since 1940")
     (:key . "h")
     (:sections
      ((:name . "Select Coordinates and Time")
       (:fields
        ("start_date"
         (:name . "Start date")
         (:type . date))
        ("end_date"
         (:name . "End date")
         (:type . date))
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))
        ("timezone"
         (:name . "Timezone")
         (:type . timezone))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Additional Variables")
         (:fields
          ("is_day"
           (:name . "Is Day or Night")
           (:type . checkbox))
          ("sunshine_duration"
           (:name . "Sunshine Duration")
           (:type . checkbox))))
        ((:name . "Solar Radiation Variables")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation GHI")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation DHI")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("global_tilted_irradiance"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation"
           (:name . "Terrestrial Solar Radiation")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation GHI (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation DHI (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox))
          ("global_tilted_irradiance_instant"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("terrestrial_radiation_instant"
           (:name . "Terrestrial Solar Radiation (Instant)")
           (:type . checkbox))
          ("tilt"
           (:name . "Panel Tilt (0° horizontal)")
           (:type . number))
          ("azimuth"
           (:name . "Panel Azimuth (0° S, -90° E, 90° W)")
           (:type . number)))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dew_point_2m"
         (:name . "Dewpoint (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation (rain + snow)")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("snow_depth"
         (:name . "Snow depth")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloud_cover"
         (:name . "Cloud cover Total")
         (:type . checkbox))
        ("cloud_cover_low"
         (:name . "Cloud cover Low")
         (:type . checkbox))
        ("cloud_cover_mid"
         (:name . "Cloud cover Mid")
         (:type . checkbox))
        ("cloud_cover_high"
         (:name . "Cloud cover High")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))
        ("vapour_pressure_deficit"
         (:name . "Vapour Pressure Deficit")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_speed_100m"
         (:name . "Wind Speed (100 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_direction_100m"
         (:name . "Wind Direction (100 m)")
         (:type . checkbox))
        ("wind_gusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))
        ("soil_temperature_0_to_7cm"
         (:name . "Soil Temperature (0-7 cm)")
         (:type . checkbox))
        ("soil_temperature_7_to_28cm"
         (:name . "Soil Temperature (7-28 cm)")
         (:type . checkbox))
        ("soil_temperature_28_to_100cm"
         (:name . "Soil Temperature (28-100 cm)")
         (:type . checkbox))
        ("soil_temperature_100_to_255cm"
         (:name . "Soil Temperature (100-255 cm)")
         (:type . checkbox))
        ("soil_moisture_0_to_7cm"
         (:name . "Soil Moisture (0-7 cm)")
         (:type . checkbox))
        ("soil_moisture_7_to_28cm"
         (:name . "Soil Moisture (7-28 cm)")
         (:type . checkbox))
        ("soil_moisture_28_to_100cm"
         (:name . "Soil Moisture (28-100 cm)")
         (:type . checkbox))
        ("soil_moisture_100_to_255cm"
         (:name . "Soil Moisture (100-255 cm)")
         (:type . checkbox))))
      ((:param . "daily")
       (:name . "Daily Weather Variables")
       (:fields
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("temperature_2m_max"
         (:name . "Maximum Temperature (2 m)")
         (:type . checkbox))
        ("temperature_2m_min"
         (:name . "Minimum Temperature (2 m)")
         (:type . checkbox))
        ("temperature_2m_mean"
         (:name . "Mean Temperature (2 m)")
         (:type . checkbox))
        ("apparent_temperature_max"
         (:name . "Maximum Apparent Temperature (2 m)")
         (:type . checkbox))
        ("apparent_temperature_min"
         (:name . "Minimum Apparent Temperature (2 m)")
         (:type . checkbox))
        ("apparent_temperature_mean"
         (:name . "Mean Apparent Temperature (2 m)")
         (:type . checkbox))
        ("sunrise"
         (:name . "Sunrise")
         (:type . checkbox))
        ("sunset"
         (:name . "Sunset")
         (:type . checkbox))
        ("daylight_duration"
         (:name . "Daylight Duration")
         (:type . checkbox))
        ("sunshine_duration"
         (:name . "Sunshine Duration")
         (:type . checkbox))
        ("precipitation_sum"
         (:name . "Precipitation Sum")
         (:type . checkbox))
        ("rain_sum"
         (:name . "Rain Sum")
         (:type . checkbox))
        ("snowfall_sum"
         (:name . "Snowfall Sum")
         (:type . checkbox))
        ("precipitation_hours"
         (:name . "Precipitation Hours")
         (:type . checkbox))
        ("wind_speed_10m_max"
         (:name . "Maximum Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_gusts_10m_max"
         (:name . "Maximum Wind Gusts (10 m)")
         (:type . checkbox))
        ("wind_direction_10m_dominant"
         (:name . "Dominant Wind Direction (10 m)")
         (:type . checkbox))
        ("shortwave_radiation_sum"
         (:name . "Shortwave Radiation Sum")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))))
      ((:name . "Settings")
       (:fields
        ("elevation"
         (:name . "Elevation")
         (:type . float))
        ("temperature_unit"
         (:name . "Temperature Unit")
         (:type . select)
         (:options
          ("celsius" . "Celsius °C")
          ("fahrenheit" . "Fahrenheit °F")))
        ("wind_speed_unit"
         (:name . "Wind Speed Unit")
         (:type . select)
         (:options
          ("kmh" . "Km/h")
          ("ms" . "m/s")
          ("mph" . "Mph")
          ("kn" . "Knots")))
        ("precipitation_unit"
         (:name . "Precipitation Unit")
         (:type . select)
         (:options
          ("mm" . "Millimeter")
          ("inch" . "Inch")))
        ("timeformat"
         (:name . "Timeformat")
         (:type . select)
         (:options
          ("iso8601" . "ISO 8601 (e.g. 2022-12-31)")
          ("unixtime" . "Unix timestamp")))))
      ((:param . "models")
       (:name . "Reanalysis models")
       (:fields
        ("best_match"
         (:name . "Best match (ECMWF IFS & ERA5)")
         (:type . checkbox))
        ("ecmwf_ifs"
         (:name . "ECMWF IFS (9 km, Global, 2017 onwards)")
         (:type . checkbox))
        ("era5_seamless"
         (:name . "ERA5-Seamless (ERA5 & ERA5-Land combined)")
         (:type . checkbox))
        ("era5"
         (:name . "ERA5 (25 km, Global)")
         (:type . checkbox))
        ("era5_land"
         (:name . "ERA5-Land (10 km, Global)")
         (:type . checkbox))
        ("cerra"
         (:name . "CERRA (5 km, Europe, 1985 to June 2021)")
         (:type . checkbox))))))
    ("Ensemble Models"
     (:name . "Ensemble Models")
     (:url . "https://open-meteo.com/en/docs/ensemble-api")
     (:description . "Weather information since 1940")
     (:key . "e")
     (:sections
      ((:name . "Select Coordinates and Time")
       (:fields
        ("end_date"
         (:name . "End date")
         (:type . date))
        ("start_date"
         (:name . "Start date")
         (:type . date))
        ("forecast_days"
         (:name . "Forecast days")
         (:type . number)
         (:min . 0)
         (:max . 16))
        ("past_days"
         (:name . "Past days")
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))
        ("timezone"
         (:name . "Timezone")
         (:type . timezone))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Additional Variables And Options")
         (:fields
          ("uv_index"
           (:name . "UV Index")
           (:type . checkbox))
          ("uv_index_clear_sky"
           (:name . "UV Index Clear Sky")
           (:type . checkbox))
          ("temperature_500hPa"
           (:name . "Temperature (500 hPa)")
           (:type . checkbox))
          ("temperature_850hPa"
           (:name . "Temperature (850 hPa)")
           (:type . checkbox))
          ("geopotential_height_500hPa"
           (:name . "Geopotential Height (500 hPa)")
           (:type . checkbox))
          ("geopotential_height_850hPa"
           (:name . "Geopotential Height (850 hPa)")
           (:type . checkbox))
          ("cape"
           (:name . "CAPE")
           (:type . checkbox))
          ("freezing_level_height"
           (:name . "Freezing Level Height")
           (:type . checkbox))
          ("sunshine_duration"
           (:name . "Sunshine Duration")
           (:type . checkbox))
          ("forecast_hours"
           (:name . "Forecast Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))
          ("past_hours"
           (:name . "Past Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))))
        ((:name . "Solar Radiation Variables")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation GHI")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation DHI")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("global_tilted_irradiance"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation GHI (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation DHI (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox))
          ("global_tilted_irradiance_instant"
           (:name . "Global Tilted Radiation GTI")
           (:type . checkbox))
          ("tilt"
           (:name . "Panel Tilt (0° horizontal)")
           (:type . number))
          ("azimuth"
           (:name . "Panel Azimuth (0° S, -90° E, 90° W)")
           (:type . number)))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dew_point_2m"
         (:name . "Dewpoint (2 m)")
         (:type . checkbox))
        ("apparent_temperature"
         (:name . "Apparent Temperature")
         (:type . checkbox))
        ("precipitation"
         (:name . "Precipitation (rain + snow)")
         (:type . checkbox))
        ("rain"
         (:name . "Rain")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("snow_depth"
         (:name . "Snow Depth")
         (:type . checkbox))
        ("weather_code"
         (:name . "Weather code")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloud_cover"
         (:name . "Cloud cover Total")
         (:type . checkbox))
        ("visibility"
         (:name . "Visibility")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))
        ("vapour_pressure_deficit"
         (:name . "Vapour Pressure Deficit")
         (:type . checkbox))
        ("wind_speed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_speed_80m"
         (:name . "Wind Speed (80 m)")
         (:type . checkbox))
        ("wind_speed_120m"
         (:name . "Wind Speed (120 m)")
         (:type . checkbox))
        ("wind_direction_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("wind_direction_80m"
         (:name . "Wind Direction (80 m)")
         (:type . checkbox))
        ("wind_direction_120m"
         (:name . "Wind Direction (120 m)")
         (:type . checkbox))
        ("wind_gusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))
        ("temperature_80m"
         (:name . "Temperature (80 m)")
         (:type . checkbox))
        ("temperature_120m"
         (:name . "Temperature (120 m)")
         (:type . checkbox))
        ("surface_temperature"
         (:name . "Surface Temperature")
         (:type . checkbox))
        ("soil_temperature_0_to_10cm"
         (:name . "Soil Temperature (0-10 cm)")
         (:type . checkbox))
        ("soil_temperature_10_to_40cm"
         (:name . "Soil Temperature (10-40 cm)")
         (:type . checkbox))
        ("soil_temperature_40_to_100cm"
         (:name . "Soil Temperature (40-100 cm)")
         (:type . checkbox))
        ("soil_temperature_100_to_200cm"
         (:name . "Soil Temperature (100-200 cm)")
         (:type . checkbox))
        ("soil_moisture_0_to_10cm"
         (:name . "Soil Moisture (0-10 cm)")
         (:type . checkbox))
        ("soil_moisture_10_to_40cm"
         (:name . "Soil Moisture (10-40 cm)")
         (:type . checkbox))
        ("soil_moisture_40_to_100cm"
         (:name . "Soil Moisture (40-100 cm)")
         (:type . checkbox))
        ("soil_moisture_100_to_200cm"
         (:name . "Soil Moisture (100-400 cm)")
         (:type . checkbox))))
      ((:name . "Settings")
       (:fields
        ("elevation"
         (:name . "Elevation")
         (:type . float))
        ("temperature_unit"
         (:name . "Temperature Unit")
         (:type . select)
         (:options
          ("celsius" . "Celsius °C")
          ("fahrenheit" . "Fahrenheit °F")))
        ("wind_speed_unit"
         (:name . "Wind Speed Unit")
         (:type . select)
         (:options
          ("kmh" . "Km/h")
          ("ms" . "m/s")
          ("mph" . "Mph")
          ("kn" . "Knots")))
        ("precipitation_unit"
         (:name . "Precipitation Unit")
         (:type . select)
         (:options
          ("mm" . "Millimeter")
          ("inch" . "Inch")))
        ("timeformat"
         (:name . "Timeformat")
         (:type . select)
         (:options
          ("iso8601" . "ISO 8601 (e.g. 2022-12-31)")
          ("unixtime" . "Unix timestamp")))))
      ((:param . "models")
       (:name . "Ensemble Models")
       (:fields
        ("icon_seamless"
         (:name . "DWD ICON EPS Seamless")
         (:type . checkbox))
        ("icon_global"
         (:name . "DWD ICON EPS Global")
         (:type . checkbox))
        ("icon_eu"
         (:name . "DWD ICON EPS EU")
         (:type . checkbox))
        ("icon_d2"
         (:name . "DWD ICON EPS D2")
         (:type . checkbox))
        ("gfs_seamless"
         (:name . "GFS Ensemble Seamless")
         (:type . checkbox))
        ("gfs025"
         (:name . "GFS Ensemble 0.25")
         (:type . checkbox))
        ("gfs05"
         (:name . "GFS Ensemble 0.5")
         (:type . checkbox))
        ("ecmwf_ifs04"
         (:name . "ECMWF IFS 0.4° Ensemble")
         (:type . checkbox))
        ("ecmwf_ifs025"
         (:name . "ECMWF IFS 0.25° Ensemble")
         (:type . checkbox))
        ("gem_global"
         (:name . "GEM Global Ensemble")
         (:type . checkbox))
        ("bom_access_global_ensemble"
         (:name . "BOM ACCESS Global")
         (:type . checkbox))))))
    ("Previous Runs API"
     (:name . "Previous Runs API")
     (:url . "https://open-meteo.com/en/docs/previous-runs-api")
     (:description . "Weather Forecasts from Previous Days to Compare Run-To-Run Performance")
     (:key . "v")
     (:sections
      ((:name . "Select Coordinates and Time")
       (:fields
        ("end_date"
         (:name . "End date")
         (:type . date))
        ("start_date"
         (:name . "Start date")
         (:type . date))
        ("forecast_days"
         (:name . "Forecast days")
         (:type . number)
         (:min . 0)
         (:max . 16))
        ("past_days"
         (:name . "Past days")
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))
        ("timezone"
         (:name . "Timezone")
         (:type . timezone))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Solar Radiation Variables")
         (:children
          ((:name . "Shortwave Solar Radiation GHI")
           (:fields
            ("shortwave_radiation"
             (:name . "Day 0")
             (:type . checkbox))
            ("shortwave_radiation_previous_day1"
             (:name . "Day 1")
             (:type . checkbox))
            ("shortwave_radiation_previous_day2"
             (:name . "Day 2")
             (:type . checkbox))
            ("shortwave_radiation_previous_day3"
             (:name . "Day 3")
             (:type . checkbox))
            ("shortwave_radiation_previous_day4"
             (:name . "Day 4")
             (:type . checkbox))
            ("shortwave_radiation_previous_day5"
             (:name . "Day 5")
             (:type . checkbox))
            ("shortwave_radiation_previous_day6"
             (:name . "Day 6")
             (:type . checkbox))
            ("shortwave_radiation_previous_day7"
             (:name . "Day 7")
             (:type . checkbox))))
          ((:name . "Direct Solar Radiation")
           (:fields
            ("direct_radiation"
             (:name . "Day 0")
             (:type . checkbox))
            ("direct_radiation_previous_day1"
             (:name . "Day 1")
             (:type . checkbox))
            ("direct_radiation_previous_day2"
             (:name . "Day 2")
             (:type . checkbox))
            ("direct_radiation_previous_day3"
             (:name . "Day 3")
             (:type . checkbox))
            ("direct_radiation_previous_day4"
             (:name . "Day 4")
             (:type . checkbox))
            ("direct_radiation_previous_day5"
             (:name . "Day 5")
             (:type . checkbox))
            ("direct_radiation_previous_day6"
             (:name . "Day 6")
             (:type . checkbox))
            ("direct_radiation_previous_day7"
             (:name . "Day 7")
             (:type . checkbox))))
          ((:name . "Diffuse Solar Radiation DHI")
           (:fields
            ("diffuse_radiation"
             (:name . "Day 0")
             (:type . checkbox))
            ("diffuse_radiation_previous_day1"
             (:name . "Day 1")
             (:type . checkbox))
            ("diffuse_radiation_previous_day2"
             (:name . "Day 2")
             (:type . checkbox))
            ("diffuse_radiation_previous_day3"
             (:name . "Day 3")
             (:type . checkbox))
            ("diffuse_radiation_previous_day4"
             (:name . "Day 4")
             (:type . checkbox))
            ("diffuse_radiation_previous_day5"
             (:name . "Day 5")
             (:type . checkbox))
            ("diffuse_radiation_previous_day6"
             (:name . "Day 6")
             (:type . checkbox))
            ("diffuse_radiation_previous_day7"
             (:name . "Day 7")
             (:type . checkbox))))
          ((:name . "Direct Normal Irradiance DNI")
           (:fields
            ("direct_normal_irradiance"
             (:name . "Day 0")
             (:type . checkbox))
            ("direct_normal_irradiance_previous_day1"
             (:name . "Day 1")
             (:type . checkbox))
            ("direct_normal_irradiance_previous_day2"
             (:name . "Day 2")
             (:type . checkbox))
            ("direct_normal_irradiance_previous_day3"
             (:name . "Day 3")
             (:type . checkbox))
            ("direct_normal_irradiance_previous_day4"
             (:name . "Day 4")
             (:type . checkbox))
            ("direct_normal_irradiance_previous_day5"
             (:name . "Day 5")
             (:type . checkbox))
            ("direct_normal_irradiance_previous_day6"
             (:name . "Day 6")
             (:type . checkbox))
            ("direct_normal_irradiance_previous_day7"
             (:name . "Day 7")
             (:type . checkbox))))
          ((:name . "Global Tilted Radiation GTI")
           (:fields
            ("global_tilted_irradiance"
             (:name . "Day 0")
             (:type . checkbox))
            ("global_tilted_irradiance_previous_day1"
             (:name . "Day 1")
             (:type . checkbox))
            ("global_tilted_irradiance_previous_day2"
             (:name . "Day 2")
             (:type . checkbox))
            ("global_tilted_irradiance_previous_day3"
             (:name . "Day 3")
             (:type . checkbox))
            ("global_tilted_irradiance_previous_day4"
             (:name . "Day 4")
             (:type . checkbox))
            ("global_tilted_irradiance_previous_day5"
             (:name . "Day 5")
             (:type . checkbox))
            ("global_tilted_irradiance_previous_day6"
             (:name . "Day 6")
             (:type . checkbox))
            ("global_tilted_irradiance_previous_day7"
             (:name . "Day 7")
             (:type . checkbox))))
          ((:name . "Shortwave Solar Radiation GHI (Instant)")
           (:fields
            ("shortwave_radiation_instant"
             (:name . "Day 0")
             (:type . checkbox))
            ("shortwave_radiation_instant_previous_day1"
             (:name . "Day 1")
             (:type . checkbox))
            ("shortwave_radiation_instant_previous_day2"
             (:name . "Day 2")
             (:type . checkbox))
            ("shortwave_radiation_instant_previous_day3"
             (:name . "Day 3")
             (:type . checkbox))
            ("shortwave_radiation_instant_previous_day4"
             (:name . "Day 4")
             (:type . checkbox))
            ("shortwave_radiation_instant_previous_day5"
             (:name . "Day 5")
             (:type . checkbox))
            ("shortwave_radiation_instant_previous_day6"
             (:name . "Day 6")
             (:type . checkbox))
            ("shortwave_radiation_instant_previous_day7"
             (:name . "Day 7")
             (:type . checkbox))))
          ((:name . "Direct Solar Radiation (Instant)")
           (:fields
            ("direct_radiation_instant"
             (:name . "Day 0")
             (:type . checkbox))
            ("direct_radiation_instant_previous_day1"
             (:name . "Day 1")
             (:type . checkbox))
            ("direct_radiation_instant_previous_day2"
             (:name . "Day 2")
             (:type . checkbox))
            ("direct_radiation_instant_previous_day3"
             (:name . "Day 3")
             (:type . checkbox))
            ("direct_radiation_instant_previous_day4"
             (:name . "Day 4")
             (:type . checkbox))
            ("direct_radiation_instant_previous_day5"
             (:name . "Day 5")
             (:type . checkbox))
            ("direct_radiation_instant_previous_day6"
             (:name . "Day 6")
             (:type . checkbox))
            ("direct_radiation_instant_previous_day7"
             (:name . "Day 7")
             (:type . checkbox))))
          ((:name . "Diffuse Solar Radiation DHI (Instant)")
           (:fields
            ("diffuse_radiation_instant"
             (:name . "Day 0")
             (:type . checkbox))
            ("diffuse_radiation_instant_previous_day1"
             (:name . "Day 1")
             (:type . checkbox))
            ("diffuse_radiation_instant_previous_day2"
             (:name . "Day 2")
             (:type . checkbox))
            ("diffuse_radiation_instant_previous_day3"
             (:name . "Day 3")
             (:type . checkbox))
            ("diffuse_radiation_instant_previous_day4"
             (:name . "Day 4")
             (:type . checkbox))
            ("diffuse_radiation_instant_previous_day5"
             (:name . "Day 5")
             (:type . checkbox))
            ("diffuse_radiation_instant_previous_day6"
             (:name . "Day 6")
             (:type . checkbox))
            ("diffuse_radiation_instant_previous_day7"
             (:name . "Day 7")
             (:type . checkbox))))
          ((:name . "Direct Normal Irradiance DNI (Instant)")
           (:fields
            ("direct_normal_irradiance_instant"
             (:name . "Day 0")
             (:type . checkbox))
            ("direct_normal_irradiance_instant_previous_day1"
             (:name . "Day 1")
             (:type . checkbox))
            ("direct_normal_irradiance_instant_previous_day2"
             (:name . "Day 2")
             (:type . checkbox))
            ("direct_normal_irradiance_instant_previous_day3"
             (:name . "Day 3")
             (:type . checkbox))
            ("direct_normal_irradiance_instant_previous_day4"
             (:name . "Day 4")
             (:type . checkbox))
            ("direct_normal_irradiance_instant_previous_day5"
             (:name . "Day 5")
             (:type . checkbox))
            ("direct_normal_irradiance_instant_previous_day6"
             (:name . "Day 6")
             (:type . checkbox))
            ("direct_normal_irradiance_instant_previous_day7"
             (:name . "Day 7")
             (:type . checkbox))))
          ((:name . "Global Tilted Radiation GTI")
           (:fields
            ("global_tilted_irradiance_instant"
             (:name . "Day 0")
             (:type . checkbox))
            ("global_tilted_irradiance_instant_previous_day1"
             (:name . "Day 1")
             (:type . checkbox))
            ("global_tilted_irradiance_instant_previous_day2"
             (:name . "Day 2")
             (:type . checkbox))
            ("global_tilted_irradiance_instant_previous_day3"
             (:name . "Day 3")
             (:type . checkbox))
            ("global_tilted_irradiance_instant_previous_day4"
             (:name . "Day 4")
             (:type . checkbox))
            ("global_tilted_irradiance_instant_previous_day5"
             (:name . "Day 5")
             (:type . checkbox))
            ("global_tilted_irradiance_instant_previous_day6"
             (:name . "Day 6")
             (:type . checkbox))
            ("global_tilted_irradiance_instant_previous_day7"
             (:name . "Day 7")
             (:type . checkbox))))
          ((:name . "Terrestrial Solar Radiation (Instant)")
           (:fields
            ("terrestrial_radiation_instant"
             (:name . "Day 0")
             (:type . checkbox))
            ("terrestrial_radiation_instant_previous_day1"
             (:name . "Day 1")
             (:type . checkbox))
            ("terrestrial_radiation_instant_previous_day2"
             (:name . "Day 2")
             (:type . checkbox))
            ("terrestrial_radiation_instant_previous_day3"
             (:name . "Day 3")
             (:type . checkbox))
            ("terrestrial_radiation_instant_previous_day4"
             (:name . "Day 4")
             (:type . checkbox))
            ("terrestrial_radiation_instant_previous_day5"
             (:name . "Day 5")
             (:type . checkbox))
            ("terrestrial_radiation_instant_previous_day6"
             (:name . "Day 6")
             (:type . checkbox))
            ("terrestrial_radiation_instant_previous_day7"
             (:name . "Day 7")
             (:type . checkbox))))))
        ((:name . "Wind on 80, 120 and 180 metre")
         (:children
          ((:name . "Wind Speed (80 m)")
           (:fields
            ("wind_speed_80m"
             (:name . "Day 0")
             (:type . checkbox))
            ("wind_speed_80m_previous_day1"
             (:name . "Day 1")
             (:type . checkbox))
            ("wind_speed_80m_previous_day2"
             (:name . "Day 2")
             (:type . checkbox))
            ("wind_speed_80m_previous_day3"
             (:name . "Day 3")
             (:type . checkbox))
            ("wind_speed_80m_previous_day4"
             (:name . "Day 4")
             (:type . checkbox))
            ("wind_speed_80m_previous_day5"
             (:name . "Day 5")
             (:type . checkbox))
            ("wind_speed_80m_previous_day6"
             (:name . "Day 6")
             (:type . checkbox))
            ("wind_speed_80m_previous_day7"
             (:name . "Day 7")
             (:type . checkbox))))
          ((:name . "Wind Speed (120 m)")
           (:fields
            ("wind_speed_120m"
             (:name . "Day 0")
             (:type . checkbox))
            ("wind_speed_120m_previous_day1"
             (:name . "Day 1")
             (:type . checkbox))
            ("wind_speed_120m_previous_day2"
             (:name . "Day 2")
             (:type . checkbox))
            ("wind_speed_120m_previous_day3"
             (:name . "Day 3")
             (:type . checkbox))
            ("wind_speed_120m_previous_day4"
             (:name . "Day 4")
             (:type . checkbox))
            ("wind_speed_120m_previous_day5"
             (:name . "Day 5")
             (:type . checkbox))
            ("wind_speed_120m_previous_day6"
             (:name . "Day 6")
             (:type . checkbox))
            ("wind_speed_120m_previous_day7"
             (:name . "Day 7")
             (:type . checkbox))))
          ((:name . "Wind Speed (180 m)")
           (:fields
            ("wind_speed_180m"
             (:name . "Day 0")
             (:type . checkbox))
            ("wind_speed_180m_previous_day1"
             (:name . "Day 1")
             (:type . checkbox))
            ("wind_speed_180m_previous_day2"
             (:name . "Day 2")
             (:type . checkbox))
            ("wind_speed_180m_previous_day3"
             (:name . "Day 3")
             (:type . checkbox))
            ("wind_speed_180m_previous_day4"
             (:name . "Day 4")
             (:type . checkbox))
            ("wind_speed_180m_previous_day5"
             (:name . "Day 5")
             (:type . checkbox))
            ("wind_speed_180m_previous_day6"
             (:name . "Day 6")
             (:type . checkbox))
            ("wind_speed_180m_previous_day7"
             (:name . "Day 7")
             (:type . checkbox))))
          ((:name . "Wind Direction (80 m)")
           (:fields
            ("wind_direction_80m"
             (:name . "Day 0")
             (:type . checkbox))
            ("wind_direction_80m_previous_day1"
             (:name . "Day 1")
             (:type . checkbox))
            ("wind_direction_80m_previous_day2"
             (:name . "Day 2")
             (:type . checkbox))
            ("wind_direction_80m_previous_day3"
             (:name . "Day 3")
             (:type . checkbox))
            ("wind_direction_80m_previous_day4"
             (:name . "Day 4")
             (:type . checkbox))
            ("wind_direction_80m_previous_day5"
             (:name . "Day 5")
             (:type . checkbox))
            ("wind_direction_80m_previous_day6"
             (:name . "Day 6")
             (:type . checkbox))
            ("wind_direction_80m_previous_day7"
             (:name . "Day 7")
             (:type . checkbox))))
          ((:name . "Wind Direction (120 m)")
           (:fields
            ("wind_direction_120m"
             (:name . "Day 0")
             (:type . checkbox))
            ("wind_direction_120m_previous_day1"
             (:name . "Day 1")
             (:type . checkbox))
            ("wind_direction_120m_previous_day2"
             (:name . "Day 2")
             (:type . checkbox))
            ("wind_direction_120m_previous_day3"
             (:name . "Day 3")
             (:type . checkbox))
            ("wind_direction_120m_previous_day4"
             (:name . "Day 4")
             (:type . checkbox))
            ("wind_direction_120m_previous_day5"
             (:name . "Day 5")
             (:type . checkbox))
            ("wind_direction_120m_previous_day6"
             (:name . "Day 6")
             (:type . checkbox))
            ("wind_direction_120m_previous_day7"
             (:name . "Day 7")
             (:type . checkbox))))
          ((:name . "Wind Direction (180 m)")
           (:fields
            ("wind_direction_180m"
             (:name . "Day 0")
             (:type . checkbox))
            ("wind_direction_180m_previous_day1"
             (:name . "Day 1")
             (:type . checkbox))
            ("wind_direction_180m_previous_day2"
             (:name . "Day 2")
             (:type . checkbox))
            ("wind_direction_180m_previous_day3"
             (:name . "Day 3")
             (:type . checkbox))
            ("wind_direction_180m_previous_day4"
             (:name . "Day 4")
             (:type . checkbox))
            ("wind_direction_180m_previous_day5"
             (:name . "Day 5")
             (:type . checkbox))
            ("wind_direction_180m_previous_day6"
             (:name . "Day 6")
             (:type . checkbox))
            ("wind_direction_180m_previous_day7"
             (:name . "Day 7")
             (:type . checkbox))))))
        ((:name . "Temperature (2 m)")
         (:fields
          ("temperature_2m"
           (:name . "Day 0")
           (:type . checkbox))
          ("temperature_2m_previous_day1"
           (:name . "Day 1")
           (:type . checkbox))
          ("temperature_2m_previous_day2"
           (:name . "Day 2")
           (:type . checkbox))
          ("temperature_2m_previous_day3"
           (:name . "Day 3")
           (:type . checkbox))
          ("temperature_2m_previous_day4"
           (:name . "Day 4")
           (:type . checkbox))
          ("temperature_2m_previous_day5"
           (:name . "Day 5")
           (:type . checkbox))
          ("temperature_2m_previous_day6"
           (:name . "Day 6")
           (:type . checkbox))
          ("temperature_2m_previous_day7"
           (:name . "Day 7")
           (:type . checkbox))))
        ((:name . "Relative Humidity (2 m)")
         (:fields
          ("relative_humidity_2m"
           (:name . "Day 0")
           (:type . checkbox))
          ("relative_humidity_2m_previous_day1"
           (:name . "Day 1")
           (:type . checkbox))
          ("relative_humidity_2m_previous_day2"
           (:name . "Day 2")
           (:type . checkbox))
          ("relative_humidity_2m_previous_day3"
           (:name . "Day 3")
           (:type . checkbox))
          ("relative_humidity_2m_previous_day4"
           (:name . "Day 4")
           (:type . checkbox))
          ("relative_humidity_2m_previous_day5"
           (:name . "Day 5")
           (:type . checkbox))
          ("relative_humidity_2m_previous_day6"
           (:name . "Day 6")
           (:type . checkbox))
          ("relative_humidity_2m_previous_day7"
           (:name . "Day 7")
           (:type . checkbox))))
        ((:name . "Dewpoint (2 m)")
         (:fields
          ("dew_point_2m"
           (:name . "Day 0")
           (:type . checkbox))
          ("dew_point_2m_previous_day1"
           (:name . "Day 1")
           (:type . checkbox))
          ("dew_point_2m_previous_day2"
           (:name . "Day 2")
           (:type . checkbox))
          ("dew_point_2m_previous_day3"
           (:name . "Day 3")
           (:type . checkbox))
          ("dew_point_2m_previous_day4"
           (:name . "Day 4")
           (:type . checkbox))
          ("dew_point_2m_previous_day5"
           (:name . "Day 5")
           (:type . checkbox))
          ("dew_point_2m_previous_day6"
           (:name . "Day 6")
           (:type . checkbox))
          ("dew_point_2m_previous_day7"
           (:name . "Day 7")
           (:type . checkbox))))
        ((:name . "Apparent Temperature")
         (:fields
          ("apparent_temperature"
           (:name . "Day 0")
           (:type . checkbox))
          ("apparent_temperature_previous_day1"
           (:name . "Day 1")
           (:type . checkbox))
          ("apparent_temperature_previous_day2"
           (:name . "Day 2")
           (:type . checkbox))
          ("apparent_temperature_previous_day3"
           (:name . "Day 3")
           (:type . checkbox))
          ("apparent_temperature_previous_day4"
           (:name . "Day 4")
           (:type . checkbox))
          ("apparent_temperature_previous_day5"
           (:name . "Day 5")
           (:type . checkbox))
          ("apparent_temperature_previous_day6"
           (:name . "Day 6")
           (:type . checkbox))
          ("apparent_temperature_previous_day7"
           (:name . "Day 7")
           (:type . checkbox))))
        ((:name . "Precipitation (rain + showers + snow)")
         (:fields
          ("precipitation"
           (:name . "Day 0")
           (:type . checkbox))
          ("precipitation_previous_day1"
           (:name . "Day 1")
           (:type . checkbox))
          ("precipitation_previous_day2"
           (:name . "Day 2")
           (:type . checkbox))
          ("precipitation_previous_day3"
           (:name . "Day 3")
           (:type . checkbox))
          ("precipitation_previous_day4"
           (:name . "Day 4")
           (:type . checkbox))
          ("precipitation_previous_day5"
           (:name . "Day 5")
           (:type . checkbox))
          ("precipitation_previous_day6"
           (:name . "Day 6")
           (:type . checkbox))
          ("precipitation_previous_day7"
           (:name . "Day 7")
           (:type . checkbox))))
        ((:name . "Rain")
         (:fields
          ("rain"
           (:name . "Day 0")
           (:type . checkbox))
          ("rain_previous_day1"
           (:name . "Day 1")
           (:type . checkbox))
          ("rain_previous_day2"
           (:name . "Day 2")
           (:type . checkbox))
          ("rain_previous_day3"
           (:name . "Day 3")
           (:type . checkbox))
          ("rain_previous_day4"
           (:name . "Day 4")
           (:type . checkbox))
          ("rain_previous_day5"
           (:name . "Day 5")
           (:type . checkbox))
          ("rain_previous_day6"
           (:name . "Day 6")
           (:type . checkbox))
          ("rain_previous_day7"
           (:name . "Day 7")
           (:type . checkbox))))
        ((:name . "Showers")
         (:fields
          ("showers"
           (:name . "Day 0")
           (:type . checkbox))
          ("showers_previous_day1"
           (:name . "Day 1")
           (:type . checkbox))
          ("showers_previous_day2"
           (:name . "Day 2")
           (:type . checkbox))
          ("showers_previous_day3"
           (:name . "Day 3")
           (:type . checkbox))
          ("showers_previous_day4"
           (:name . "Day 4")
           (:type . checkbox))
          ("showers_previous_day5"
           (:name . "Day 5")
           (:type . checkbox))
          ("showers_previous_day6"
           (:name . "Day 6")
           (:type . checkbox))
          ("showers_previous_day7"
           (:name . "Day 7")
           (:type . checkbox))))
        ((:name . "Snowfall")
         (:fields
          ("snowfall"
           (:name . "Day 0")
           (:type . checkbox))
          ("snowfall_previous_day1"
           (:name . "Day 1")
           (:type . checkbox))
          ("snowfall_previous_day2"
           (:name . "Day 2")
           (:type . checkbox))
          ("snowfall_previous_day3"
           (:name . "Day 3")
           (:type . checkbox))
          ("snowfall_previous_day4"
           (:name . "Day 4")
           (:type . checkbox))
          ("snowfall_previous_day5"
           (:name . "Day 5")
           (:type . checkbox))
          ("snowfall_previous_day6"
           (:name . "Day 6")
           (:type . checkbox))
          ("snowfall_previous_day7"
           (:name . "Day 7")
           (:type . checkbox))))
        ((:name . "Weather code")
         (:fields
          ("weather_code"
           (:name . "Day 0")
           (:type . checkbox))
          ("weather_code_previous_day1"
           (:name . "Day 1")
           (:type . checkbox))
          ("weather_code_previous_day2"
           (:name . "Day 2")
           (:type . checkbox))
          ("weather_code_previous_day3"
           (:name . "Day 3")
           (:type . checkbox))
          ("weather_code_previous_day4"
           (:name . "Day 4")
           (:type . checkbox))
          ("weather_code_previous_day5"
           (:name . "Day 5")
           (:type . checkbox))
          ("weather_code_previous_day6"
           (:name . "Day 6")
           (:type . checkbox))
          ("weather_code_previous_day7"
           (:name . "Day 7")
           (:type . checkbox))))
        ((:name . "Sealevel Pressure")
         (:fields
          ("pressure_msl"
           (:name . "Day 0")
           (:type . checkbox))
          ("pressure_msl_previous_day1"
           (:name . "Day 1")
           (:type . checkbox))
          ("pressure_msl_previous_day2"
           (:name . "Day 2")
           (:type . checkbox))
          ("pressure_msl_previous_day3"
           (:name . "Day 3")
           (:type . checkbox))
          ("pressure_msl_previous_day4"
           (:name . "Day 4")
           (:type . checkbox))
          ("pressure_msl_previous_day5"
           (:name . "Day 5")
           (:type . checkbox))
          ("pressure_msl_previous_day6"
           (:name . "Day 6")
           (:type . checkbox))
          ("pressure_msl_previous_day7"
           (:name . "Day 7")
           (:type . checkbox))))
        ((:name . "Surface Pressure")
         (:fields
          ("surface_pressure"
           (:name . "Day 0")
           (:type . checkbox))
          ("surface_pressure_previous_day1"
           (:name . "Day 1")
           (:type . checkbox))
          ("surface_pressure_previous_day2"
           (:name . "Day 2")
           (:type . checkbox))
          ("surface_pressure_previous_day3"
           (:name . "Day 3")
           (:type . checkbox))
          ("surface_pressure_previous_day4"
           (:name . "Day 4")
           (:type . checkbox))
          ("surface_pressure_previous_day5"
           (:name . "Day 5")
           (:type . checkbox))
          ("surface_pressure_previous_day6"
           (:name . "Day 6")
           (:type . checkbox))
          ("surface_pressure_previous_day7"
           (:name . "Day 7")
           (:type . checkbox))))
        ((:name . "Cloud cover Total")
         (:fields
          ("cloud_cover"
           (:name . "Day 0")
           (:type . checkbox))
          ("cloud_cover_previous_day1"
           (:name . "Day 1")
           (:type . checkbox))
          ("cloud_cover_previous_day2"
           (:name . "Day 2")
           (:type . checkbox))
          ("cloud_cover_previous_day3"
           (:name . "Day 3")
           (:type . checkbox))
          ("cloud_cover_previous_day4"
           (:name . "Day 4")
           (:type . checkbox))
          ("cloud_cover_previous_day5"
           (:name . "Day 5")
           (:type . checkbox))
          ("cloud_cover_previous_day6"
           (:name . "Day 6")
           (:type . checkbox))
          ("cloud_cover_previous_day7"
           (:name . "Day 7")
           (:type . checkbox))))
        ((:name . "Wind Speed (10 m)")
         (:fields
          ("wind_speed_10m"
           (:name . "Day 0")
           (:type . checkbox))
          ("wind_speed_10m_previous_day1"
           (:name . "Day 1")
           (:type . checkbox))
          ("wind_speed_10m_previous_day2"
           (:name . "Day 2")
           (:type . checkbox))
          ("wind_speed_10m_previous_day3"
           (:name . "Day 3")
           (:type . checkbox))
          ("wind_speed_10m_previous_day4"
           (:name . "Day 4")
           (:type . checkbox))
          ("wind_speed_10m_previous_day5"
           (:name . "Day 5")
           (:type . checkbox))
          ("wind_speed_10m_previous_day6"
           (:name . "Day 6")
           (:type . checkbox))
          ("wind_speed_10m_previous_day7"
           (:name . "Day 7")
           (:type . checkbox))))
        ((:name . "Wind Direction (10 m)")
         (:fields
          ("wind_direction_10m"
           (:name . "Day 0")
           (:type . checkbox))
          ("wind_direction_10m_previous_day1"
           (:name . "Day 1")
           (:type . checkbox))
          ("wind_direction_10m_previous_day2"
           (:name . "Day 2")
           (:type . checkbox))
          ("wind_direction_10m_previous_day3"
           (:name . "Day 3")
           (:type . checkbox))
          ("wind_direction_10m_previous_day4"
           (:name . "Day 4")
           (:type . checkbox))
          ("wind_direction_10m_previous_day5"
           (:name . "Day 5")
           (:type . checkbox))
          ("wind_direction_10m_previous_day6"
           (:name . "Day 6")
           (:type . checkbox))
          ("wind_direction_10m_previous_day7"
           (:name . "Day 7")
           (:type . checkbox))))))
      ((:name . "Settings")
       (:fields
        ("temperature_unit"
         (:name . "Temperature Unit")
         (:type . select)
         (:options
          ("celsius" . "Celsius °C")
          ("fahrenheit" . "Fahrenheit °F")))
        ("wind_speed_unit"
         (:name . "Wind Speed Unit")
         (:type . select)
         (:options
          ("kmh" . "Km/h")
          ("ms" . "m/s")
          ("mph" . "Mph")
          ("kn" . "Knots")))
        ("precipitation_unit"
         (:name . "Precipitation Unit")
         (:type . select)
         (:options
          ("mm" . "Millimeter")
          ("inch" . "Inch")))
        ("timeformat"
         (:name . "Timeformat")
         (:type . select)
         (:options
          ("iso8601" . "ISO 8601 (e.g. 2022-12-31)")
          ("unixtime" . "Unix timestamp")))))
      ((:param . "models")
       (:name . "Weather models")
       (:fields
        ("best_match"
         (:name . "Best match")
         (:type . checkbox))
        ("ecmwf_ifs04"
         (:name . "ECMWF IFS 0.4°")
         (:type . checkbox))
        ("ecmwf_ifs025"
         (:name . "ECMWF IFS 0.25°")
         (:type . checkbox))
        ("ecmwf_aifs025"
         (:name . "ECMWF AIFS 0.25°")
         (:type . checkbox))
        ("cma_grapes_global"
         (:name . "CMA GRAPES Global")
         (:type . checkbox))
        ("bom_access_global"
         (:name . "BOM ACCESS Global")
         (:type . checkbox))
        ("metno_nordic"
         (:name . "MET Norway Nordic")
         (:type . checkbox))
        ("gfs_seamless"
         (:name . "GFS Seamless")
         (:type . checkbox))
        ("gfs_global"
         (:name . "GFS Global")
         (:type . checkbox))
        ("gfs_hrrr"
         (:name . "GFS HRRR")
         (:type . checkbox))
        ("gfs_graphcast025"
         (:name . "GFS GraphCast")
         (:type . checkbox))
        ("jma_seamless"
         (:name . "JMA Seamless")
         (:type . checkbox))
        ("jma_msm"
         (:name . "JMA MSM")
         (:type . checkbox))
        ("jma_gsm"
         (:name . "JMA GSM")
         (:type . checkbox))
        ("icon_seamless"
         (:name . "DWD ICON Seamless")
         (:type . checkbox))
        ("icon_global"
         (:name . "DWD ICON Global")
         (:type . checkbox))
        ("icon_eu"
         (:name . "DWD ICON EU")
         (:type . checkbox))
        ("icon_d2"
         (:name . "DWD ICON D2")
         (:type . checkbox))
        ("gem_seamless"
         (:name . "GEM Seamless")
         (:type . checkbox))
        ("gem_global"
         (:name . "GEM Global")
         (:type . checkbox))
        ("gem_regional"
         (:name . "GEM Regional")
         (:type . checkbox))
        ("gem_hrdps_continental"
         (:name . "GEM HRDPS Continental")
         (:type . checkbox))
        ("meteofrance_seamless"
         (:name . "Météo-France Seamless")
         (:type . checkbox))
        ("meteofrance_arpege_world"
         (:name . "Météo-France ARPEGE World")
         (:type . checkbox))
        ("meteofrance_arpege_europe"
         (:name . "Météo-France ARPEGE Europe")
         (:type . checkbox))
        ("meteofrance_arome_france"
         (:name . "Météo-France AROME France")
         (:type . checkbox))
        ("meteofrance_arome_france_hd"
         (:name . "Météo-France AROME France HD")
         (:type . checkbox))
        ("arpae_cosmo_seamless"
         (:name . "ARPAE Seamless")
         (:type . checkbox))
        ("arpae_cosmo_2i"
         (:name . "ARPAE COSMO 2I")
         (:type . checkbox))
        ("arpae_cosmo_2i_ruc"
         (:name . "ARPAE COSMO 2I RUC")
         (:type . checkbox))
        ("arpae_cosmo_5m"
         (:name . "ARPAE COSMO 5M")
         (:type . checkbox))))))
    ("Climate Change"
     (:name . "Climate Change")
     (:url . "https://open-meteo.com/en/docs/climate-api")
     (:description . "Climate change projections")
     (:key . "c")
     (:sections
      ((:name . "Select Coordinates and Time")
       (:fields
        ("start_date"
         (:name . "Start date")
         (:type . date))
        ("end_date"
         (:name . "End date")
         (:type . date))
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))
        ("timezone"
         (:name . "Timezone")
         (:type . timezone))))
      ((:param . "daily")
       (:name . "Daily Weather Variables")
       (:children)
       (:fields
        ("temperature_2m_mean"
         (:name . "Mean Temperature (2 m)")
         (:type . checkbox))
        ("temperature_2m_max"
         (:name . "Maximum Temperature (2 m)")
         (:type . checkbox))
        ("temperature_2m_min"
         (:name . "Minimum Temperature (2 m)")
         (:type . checkbox))
        ("wind_speed_10m_mean"
         (:name . "Mean Wind Speed (10 m)")
         (:type . checkbox))
        ("wind_speed_10m_max"
         (:name . "Max Wind Speed (10 m)")
         (:type . checkbox))
        ("cloud_cover_mean"
         (:name . "Mean Cloud Cover")
         (:type . checkbox))
        ("shortwave_radiation_sum"
         (:name . "Shortwave Radiation Sum")
         (:type . checkbox))
        ("relative_humidity_2m_mean"
         (:name . "Mean Relative Humidity (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m_max"
         (:name . "Maximum Relative Humidity (2 m)")
         (:type . checkbox))
        ("relative_humidity_2m_min"
         (:name . "Minimum Relative Humidity (2 m)")
         (:type . checkbox))
        ("dew_point_2m_mean"
         (:name . "Mean Dewpoint (2 m)")
         (:type . checkbox))
        ("dew_point_2m_min"
         (:name . "Minimum Dewpoint (2 m)")
         (:type . checkbox))
        ("dew_point_2m_max"
         (:name . "Maximum Dewpoint (2 m)")
         (:type . checkbox))
        ("precipitation_sum"
         (:name . "Precipitation Sum")
         (:type . checkbox))
        ("rain_sum"
         (:name . "Rain Sum")
         (:type . checkbox))
        ("snowfall_sum"
         (:name . "Snowfall Sum")
         (:type . checkbox))
        ("pressure_msl_mean"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("soil_moisture_0_to_10cm_mean"
         (:name . "Mean Soil Moisture (0-10 cm)")
         (:type . checkbox))
        ("et0_fao_evapotranspiration_sum"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))))
      ((:name . "Settings")
       (:fields
        ("disable_bias_correction"
         (:name . "Raw data. Disable statistical downscaling with ERA5-Land (10 km)")
         (:type . checkbox))
        ("temperature_unit"
         (:name . "Temperature Unit")
         (:type . select)
         (:options
          ("celsius" . "Celsius °C")
          ("fahrenheit" . "Fahrenheit °F")))
        ("wind_speed_unit"
         (:name . "Wind Speed Unit")
         (:type . select)
         (:options
          ("kmh" . "Km/h")
          ("ms" . "m/s")
          ("mph" . "Mph")
          ("kn" . "Knots")))
        ("precipitation_unit"
         (:name . "Precipitation Unit")
         (:type . select)
         (:options
          ("mm" . "Millimeter")
          ("inch" . "Inch")))
        ("timeformat"
         (:name . "Timeformat")
         (:type . select)
         (:options
          ("iso8601" . "ISO 8601 (e.g. 2022-12-31)")
          ("unixtime" . "Unix timestamp")))))
      ((:param . "models")
       (:name . "Climate models")
       (:fields
        ("CMCC_CM2_VHR4"
         (:name . "CMCC_CM2_VHR4 (30 km)")
         (:type . checkbox))
        ("FGOALS_f3_H"
         (:name . "FGOALS_f3_H (28 km)")
         (:type . checkbox))
        ("HiRAM_SIT_HR"
         (:name . "HiRAM_SIT_HR (25 km)")
         (:type . checkbox))
        ("MRI_AGCM3_2_S"
         (:name . "MRI_AGCM3_2_S (20 km)")
         (:type . checkbox))
        ("EC_Earth3P_HR"
         (:name . "EC_Earth3P_HR (29 km)")
         (:type . checkbox))
        ("MPI_ESM1_2_XR"
         (:name . "MPI_ESM1_2_XR (51 km)")
         (:type . checkbox))
        ("NICAM16_8S"
         (:name . "NICAM16_8S (31 km)")
         (:type . checkbox))))))
    ("Marine Forecast"
     (:name . "Marine Forecast")
     (:url . "https://open-meteo.com/en/docs/marine-weather-api")
     (:description . "Wave forecasts")
     (:key . "m")
     (:sections
      ((:name . "Select Coordinates and Time")
       (:fields
        ("end_date"
         (:name . "End date")
         (:type . date))
        ("start_date"
         (:name . "Start date")
         (:type . date))
        ("forecast_days"
         (:name . "Forecast days")
         (:type . number)
         (:min . 0)
         (:max . 16))
        ("past_days"
         (:name . "Past days")
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))
        ("timezone"
         (:name . "Timezone")
         (:type . timezone))))
      ((:param . "hourly")
       (:name . "Hourly Marine Variables")
       (:children
        ((:name . "Additional Options")
         (:fields
          ("forecast_hours"
           (:name . "Forecast Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours")))
          ("past_hours"
           (:name . "Past Hours")
           (:type . select)
           (:options
            ("1" . "1 hour")
            ("6" . "6 hours")
            ("12" . "12 hours")
            ("24" . "24 hours"))))))
       (:fields
        ("wave_height"
         (:name . "Wave Height")
         (:type . checkbox))
        ("wave_direction"
         (:name . "Wave Direction")
         (:type . checkbox))
        ("wave_period"
         (:name . "Wave Period")
         (:type . checkbox))
        ("wind_wave_height"
         (:name . "Wind Wave Height")
         (:type . checkbox))
        ("wind_wave_direction"
         (:name . "Wind Wave Direction")
         (:type . checkbox))
        ("wind_wave_period"
         (:name . "Wind Wave Period")
         (:type . checkbox))
        ("wind_wave_peak_period"
         (:name . "Wind Wave Peak Period")
         (:type . checkbox))
        ("swell_wave_height"
         (:name . "Swell Wave Height")
         (:type . checkbox))
        ("swell_wave_direction"
         (:name . "Swell Wave Direction")
         (:type . checkbox))
        ("swell_wave_period"
         (:name . "Swell Wave Period")
         (:type . checkbox))
        ("swell_wave_peak_period"
         (:name . "Swell Wave Peak Period")
         (:type . checkbox))))
      ((:param . "daily")
       (:name . "Daily Marine Variables")
       (:fields
        ("wave_height_max"
         (:name . "Wave Height Max")
         (:type . checkbox))
        ("wave_direction_dominant"
         (:name . "Wave Direction Dominant")
         (:type . checkbox))
        ("wave_period_max"
         (:name . "Wave Period Max")
         (:type . checkbox))
        ("wind_wave_height_max"
         (:name . "Wind Wave Height Max")
         (:type . checkbox))
        ("wind_wave_direction_dominant"
         (:name . "Wind Wave Direction Dominant")
         (:type . checkbox))
        ("wind_wave_period_max"
         (:name . "Wind Wave Period Max")
         (:type . checkbox))
        ("wind_wave_peak_period_max"
         (:name . "Wind Wave Peak Period Max")
         (:type . checkbox))
        ("swell_wave_height_max"
         (:name . "Swell Wave Height Max")
         (:type . checkbox))
        ("swell_wave_direction_dominant"
         (:name . "Swell Wave Direction Dominant")
         (:type . checkbox))
        ("swell_wave_period_max"
         (:name . "Swell Wave Period Max")
         (:type . checkbox))
        ("swell_wave_peak_period_max"
         (:name . "Swell Wave Peak Period Max")
         (:type . checkbox))))
      ((:name . "Current Conditions")
       (:fields
        ("wave_height"
         (:name . "Wave Height")
         (:type . checkbox))
        ("wave_direction"
         (:name . "Wave Direction")
         (:type . checkbox))
        ("wave_period"
         (:name . "Wave Period")
         (:type . checkbox))
        ("wind_wave_height"
         (:name . "Wind Wave Height")
         (:type . checkbox))
        ("wind_wave_direction"
         (:name . "Wind Wave Direction")
         (:type . checkbox))
        ("wind_wave_period"
         (:name . "Wind Wave Period")
         (:type . checkbox))
        ("wind_wave_peak_period"
         (:name . "Wind Wave Peak Period")
         (:type . checkbox))
        ("swell_wave_height"
         (:name . "Swell Wave Height")
         (:type . checkbox))
        ("swell_wave_direction"
         (:name . "Swell Wave Direction")
         (:type . checkbox))
        ("swell_wave_period"
         (:name . "Swell Wave Period")
         (:type . checkbox))
        ("swell_wave_peak_period"
         (:name . "Swell Wave Peak Period")
         (:type . checkbox))))
      ((:name . "Settings")
       (:fields
        ("length_unit"
         (:name . "Length Unit")
         (:type . select)
         (:options
          ("metric" . "Metric")
          ("imperial" . "Imperial")))
        ("timeformat"
         (:name . "Timeformat")
         (:type . select)
         (:options
          ("iso8601" . "ISO 8601 (e.g. 2022-12-31)")
          ("unixtime" . "Unix timestamp")))))
      ((:name . "Wave Models")
       (:fields
        ("best_match"
         (:name . "Best match (EWAM & GWAM)")
         (:type . checkbox))
        ("ewam"
         (:name . "DWD EWAM (0.05° only Europe)")
         (:type . checkbox))
        ("gwam"
         (:name . "DWD GWAM (0.25°)")
         (:type . checkbox))
        ("era5_ocean"
         (:name . "ERA5-Ocean (0.5°, data from 1940 onwards)")
         (:type . checkbox))))))
    ("Air Quality"
     (:name . "Air Quality")
     (:url . "https://open-meteo.com/en/docs/air-quality-api")
     (:description . "Pollutants and pollen forcast")
     (:key . "a")
     (:sections
      ((:name . "Select Coordinates and Time")
       (:fields
        ("forecast_days"
         (:name . "Forecast days")
         (:type . number)
         (:min . 0)
         (:max . 16))
        ("past_days"
         (:name . "Past days")
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))
        ("timezone"
         (:name . "Timezone")
         (:type . timezone))))
      ((:param . "hourly")
       (:name . "Hourly Air Quality Variables")
       (:children
        ((:name . "European Air Quality Index")
         (:fields))
        ((:name . "United States Air Quality Index")
         (:fields))
        ((:name . "Additional Options")
         (:fields)))
       (:fields
        ("pm10"
         (:name . "Particulate Matter PM 10")
         (:type . checkbox))
        ("pm2_5"
         (:name . "Particulate Matter PM 2.5")
         (:type . checkbox))
        ("carbon_monoxide"
         (:name . "Carbon Monoxide CO")
         (:type . checkbox))
        ("nitrogen_dioxide"
         (:name . "Nitrogen Dioxide NO 2")
         (:type . checkbox))
        ("sulphur_dioxide"
         (:name . "Sulphur Dioxide SO 2")
         (:type . checkbox))
        ("ozone"
         (:name . "Ozone O 3")
         (:type . checkbox))
        ("aerosol_optical_depth"
         (:name . "Aerosol Optical Depth")
         (:type . checkbox))
        ("dust"
         (:name . "Dust")
         (:type . checkbox))
        ("uv_index"
         (:name . "UV Index")
         (:type . checkbox))
        ("uv_index_clear_sky"
         (:name . "UV Index Clear Sky")
         (:type . checkbox))
        ("ammonia"
         (:name . "Ammonia NH 3 (*)")
         (:type . checkbox))
        ("alder_pollen"
         (:name . "Alder Pollen (*)")
         (:type . checkbox))
        ("birch_pollen"
         (:name . "Birch Pollen (*)")
         (:type . checkbox))
        ("grass_pollen"
         (:name . "Grass Pollen (*)")
         (:type . checkbox))
        ("mugwort_pollen"
         (:name . "Mugwort Pollen (*)")
         (:type . checkbox))
        ("olive_pollen"
         (:name . "Olive Pollen (*)")
         (:type . checkbox))
        ("ragweed_pollen"
         (:name . "Ragweed Pollen (*)")
         (:type . checkbox))))
      ((:name . "Current Conditions")
       (:fields
        ("european_aqi"
         (:name . "European AQI")
         (:type . checkbox))
        ("us_aqi"
         (:name . "United States AQI")
         (:type . checkbox))
        ("pm10"
         (:name . "Particulate Matter PM 10")
         (:type . checkbox))
        ("pm2_5"
         (:name . "Particulate Matter PM 2.5")
         (:type . checkbox))
        ("carbon_monoxide"
         (:name . "Carbon Monoxide CO")
         (:type . checkbox))
        ("nitrogen_dioxide"
         (:name . "Nitrogen Dioxide NO 2")
         (:type . checkbox))
        ("sulphur_dioxide"
         (:name . "Sulphur Dioxide SO 2")
         (:type . checkbox))
        ("ozone"
         (:name . "Ozone O 3")
         (:type . checkbox))
        ("aerosol_optical_depth"
         (:name . "Aerosol Optical Depth")
         (:type . checkbox))
        ("dust"
         (:name . "Dust")
         (:type . checkbox))
        ("uv_index"
         (:name . "UV Index")
         (:type . checkbox))
        ("uv_index_clear_sky"
         (:name . "UV Index Clear Sky")
         (:type . checkbox))
        ("ammonia"
         (:name . "Ammonia NH 3 (*)")
         (:type . checkbox))
        ("alder_pollen"
         (:name . "Alder Pollen (*)")
         (:type . checkbox))
        ("birch_pollen"
         (:name . "Birch Pollen (*)")
         (:type . checkbox))
        ("grass_pollen"
         (:name . "Grass Pollen (*)")
         (:type . checkbox))
        ("mugwort_pollen"
         (:name . "Mugwort Pollen (*)")
         (:type . checkbox))
        ("olive_pollen"
         (:name . "Olive Pollen (*)")
         (:type . checkbox))
        ("ragweed_pollen"
         (:name . "Ragweed Pollen (*)")
         (:type . checkbox))))
      ((:name . "Settings")
       (:fields
        ("domains"
         (:name . "Domain")
         (:type . select)
         (:options
          ("auto" . "Global + European")
          ("cams_global" . "Global (40 km)")
          ("cams_europe" . "European (11 km)")))
        ("timeformat"
         (:name . "Timeformat")
         (:type . select)
         (:options
          ("iso8601" . "ISO 8601 (e.g. 2022-12-31)")
          ("unixtime" . "Unix timestamp")))))))
    ("Flood"
     (:name . "Flood")
     (:url . "https://open-meteo.com/en/docs/flood-api")
     (:description . "River discharge forecast")
     (:key . "f")
     (:sections
      ((:name . "Select Coordinates and Time")
       (:fields
        ("end_date"
         (:name . "End date")
         (:type . date))
        ("start_date"
         (:name . "Start date")
         (:type . date))
        ("forecast_days"
         (:name . "Forecast days")
         (:type . number)
         (:min . 0)
         (:max . 210))
        ("past_days"
         (:name . "Past days")
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))
        ("timezone"
         (:name . "Timezone")
         (:type . timezone))))
      ((:param . "daily")
       (:name . "Daily Weather Variables")
       (:children)
       (:fields
        ("river_discharge"
         (:name . "River Discharge")
         (:type . checkbox))
        ("river_discharge_mean"
         (:name . "River Discharge Mean")
         (:type . checkbox))
        ("river_discharge_median"
         (:name . "River Discharge Median")
         (:type . checkbox))
        ("river_discharge_max"
         (:name . "River Discharge Maximum")
         (:type . checkbox))
        ("river_discharge_min"
         (:name . "River Discharge Minimum")
         (:type . checkbox))
        ("river_discharge_p25"
         (:name . "River Discharge 25 th Percentile")
         (:type . checkbox))
        ("river_discharge_p75"
         (:name . "River Discharge 75 th Percentile")
         (:type . checkbox))
        ("ensemble"
         (:name . "All 50 Ensemble Members")
         (:type . checkbox))))
      ((:name . "Settings")
       (:fields
        ("timeformat"
         (:name . "Timeformat")
         (:type . select)
         (:options
          ("iso8601" . "ISO 8601 (e.g. 2022-12-31)")
          ("unixtime" . "Unix timestamp")))))
      ((:param . "models")
       (:name . "Flood Models")
       (:fields
        ("seamless_v4"
         (:name . "GloFAS v4 Seamless")
         (:type . checkbox))
        ("forecast_v4"
         (:name . "GloFAS v4 Forecast")
         (:type . checkbox))
        ("consolidated_v4"
         (:name . "GloFAS v4 Consolidated")
         (:type . checkbox))
        ("seamless_v3"
         (:name . "GloFAS v3 Seamless")
         (:type . checkbox))
        ("forecast_v3"
         (:name . "GloFAS v3 Forecast")
         (:type . checkbox))
        ("consolidated_v3"
         (:name . "GloFAS v3 Consolidated")
         (:type . checkbox)))))))
  "Open-meteo API docs data.")


(defconst biome-api-timezones
  '("Africa/Abidjan" "Africa/Accra" "Africa/Addis_Ababa"
    "Africa/Algiers" "Africa/Asmara" "Africa/Asmera" "Africa/Bamako"
    "Africa/Bangui" "Africa/Banjul" "Africa/Bissau" "Africa/Blantyre"
    "Africa/Brazzaville" "Africa/Bujumbura" "Africa/Cairo"
    "Africa/Casablanca" "Africa/Ceuta" "Africa/Conakry" "Africa/Dakar"
    "Africa/Dar_es_Salaam" "Africa/Djibouti" "Africa/Douala"
    "Africa/El_Aaiun" "Africa/Freetown" "Africa/Gaborone"
    "Africa/Harare" "Africa/Johannesburg" "Africa/Juba" "Africa/Kampala"
    "Africa/Khartoum" "Africa/Kigali" "Africa/Kinshasa" "Africa/Lagos"
    "Africa/Libreville" "Africa/Lome" "Africa/Luanda"
    "Africa/Lubumbashi" "Africa/Lusaka" "Africa/Malabo" "Africa/Maputo"
    "Africa/Maseru" "Africa/Mbabane" "Africa/Mogadishu"
    "Africa/Monrovia" "Africa/Nairobi" "Africa/Ndjamena" "Africa/Niamey"
    "Africa/Nouakchott" "Africa/Ouagadougou" "Africa/Porto-Novo"
    "Africa/Sao_Tome" "Africa/Timbuktu" "Africa/Tripoli" "Africa/Tunis"
    "Africa/Windhoek" "America/Adak" "America/Anchorage"
    "America/Anguilla" "America/Antigua" "America/Araguaina"
    "America/Argentina/Buenos_Aires" "America/Argentina/Catamarca"
    "America/Argentina/ComodRivadavia" "America/Argentina/Cordoba"
    "America/Argentina/Jujuy" "America/Argentina/La_Rioja"
    "America/Argentina/Mendoza" "America/Argentina/Rio_Gallegos"
    "America/Argentina/Salta" "America/Argentina/San_Juan"
    "America/Argentina/San_Luis" "America/Argentina/Tucuman"
    "America/Argentina/Ushuaia" "America/Aruba" "America/Asuncion"
    "America/Atikokan" "America/Atka" "America/Bahia"
    "America/Bahia_Banderas" "America/Barbados" "America/Belem"
    "America/Belize" "America/Blanc-Sablon" "America/Boa_Vista"
    "America/Bogota" "America/Boise" "America/Buenos_Aires"
    "America/Cambridge_Bay" "America/Campo_Grande" "America/Cancun"
    "America/Caracas" "America/Catamarca" "America/Cayenne"
    "America/Cayman" "America/Chicago" "America/Chihuahua"
    "America/Coral_Harbour" "America/Cordoba" "America/Costa_Rica"
    "America/Creston" "America/Cuiaba" "America/Curacao"
    "America/Danmarkshavn" "America/Dawson" "America/Dawson_Creek"
    "America/Denver" "America/Detroit" "America/Dominica"
    "America/Edmonton" "America/Eirunepe" "America/El_Salvador"
    "America/Ensenada" "America/Fort_Nelson" "America/Fort_Wayne"
    "America/Fortaleza" "America/Glace_Bay" "America/Godthab"
    "America/Goose_Bay" "America/Grand_Turk" "America/Grenada"
    "America/Guadeloupe" "America/Guatemala" "America/Guayaquil"
    "America/Guyana" "America/Halifax" "America/Havana"
    "America/Hermosillo" "America/Indiana/Indianapolis"
    "America/Indiana/Knox" "America/Indiana/Marengo"
    "America/Indiana/Petersburg" "America/Indiana/Tell_City"
    "America/Indiana/Vevay" "America/Indiana/Vincennes"
    "America/Indiana/Winamac" "America/Indianapolis" "America/Inuvik"
    "America/Iqaluit" "America/Jamaica" "America/Jujuy" "America/Juneau"
    "America/Kentucky/Louisville" "America/Kentucky/Monticello"
    "America/Knox_IN" "America/Kralendijk" "America/La_Paz"
    "America/Lima" "America/Los_Angeles" "America/Louisville"
    "America/Lower_Princes" "America/Maceio" "America/Managua"
    "America/Manaus" "America/Marigot" "America/Martinique"
    "America/Matamoros" "America/Mazatlan" "America/Mendoza"
    "America/Menominee" "America/Merida" "America/Metlakatla"
    "America/Mexico_City" "America/Miquelon" "America/Moncton"
    "America/Monterrey" "America/Montevideo" "America/Montreal"
    "America/Montserrat" "America/Nassau" "America/New_York"
    "America/Nipigon" "America/Nome" "America/Noronha"
    "America/North_Dakota/Beulah" "America/North_Dakota/Center"
    "America/North_Dakota/New_Salem" "America/Nuuk" "America/Ojinaga"
    "America/Panama" "America/Pangnirtung" "America/Paramaribo"
    "America/Phoenix" "America/Port-au-Prince" "America/Port_of_Spain"
    "America/Porto_Acre" "America/Porto_Velho" "America/Puerto_Rico"
    "America/Punta_Arenas" "America/Rainy_River" "America/Rankin_Inlet"
    "America/Recife" "America/Regina" "America/Resolute"
    "America/Rio_Branco" "America/Rosario" "America/Santa_Isabel"
    "America/Santarem" "America/Santiago" "America/Santo_Domingo"
    "America/Sao_Paulo" "America/Scoresbysund" "America/Shiprock"
    "America/Sitka" "America/St_Barthelemy" "America/St_Johns"
    "America/St_Kitts" "America/St_Lucia" "America/St_Thomas"
    "America/St_Vincent" "America/Swift_Current" "America/Tegucigalpa"
    "America/Thule" "America/Thunder_Bay" "America/Tijuana"
    "America/Toronto" "America/Tortola" "America/Vancouver"
    "America/Virgin" "America/Whitehorse" "America/Winnipeg"
    "America/Yakutat" "America/Yellowknife" "Antarctica/Casey"
    "Antarctica/Davis" "Antarctica/DumontDUrville"
    "Antarctica/Macquarie" "Antarctica/Mawson" "Antarctica/McMurdo"
    "Antarctica/Palmer" "Antarctica/Rothera" "Antarctica/South_Pole"
    "Antarctica/Syowa" "Antarctica/Troll" "Antarctica/Vostok"
    "Arctic/Longyearbyen" "Asia/Aden" "Asia/Almaty" "Asia/Amman"
    "Asia/Anadyr" "Asia/Aqtau" "Asia/Aqtobe" "Asia/Ashgabat"
    "Asia/Ashkhabad" "Asia/Atyrau" "Asia/Baghdad" "Asia/Bahrain"
    "Asia/Baku" "Asia/Bangkok" "Asia/Barnaul" "Asia/Beirut"
    "Asia/Bishkek" "Asia/Brunei" "Asia/Calcutta" "Asia/Chita"
    "Asia/Choibalsan" "Asia/Chongqing" "Asia/Chungking" "Asia/Colombo"
    "Asia/Dacca" "Asia/Damascus" "Asia/Dhaka" "Asia/Dili" "Asia/Dubai"
    "Asia/Dushanbe" "Asia/Famagusta" "Asia/Gaza" "Asia/Harbin"
    "Asia/Hebron" "Asia/Ho_Chi_Minh" "Asia/Hong_Kong" "Asia/Hovd"
    "Asia/Irkutsk" "Asia/Istanbul" "Asia/Jakarta" "Asia/Jayapura"
    "Asia/Jerusalem" "Asia/Kabul" "Asia/Kamchatka" "Asia/Karachi"
    "Asia/Kashgar" "Asia/Kathmandu" "Asia/Katmandu" "Asia/Khandyga"
    "Asia/Kolkata" "Asia/Krasnoyarsk" "Asia/Kuala_Lumpur" "Asia/Kuching"
    "Asia/Kuwait" "Asia/Macao" "Asia/Macau" "Asia/Magadan"
    "Asia/Makassar" "Asia/Manila" "Asia/Muscat" "Asia/Nicosia"
    "Asia/Novokuznetsk" "Asia/Novosibirsk" "Asia/Omsk" "Asia/Oral"
    "Asia/Phnom_Penh" "Asia/Pontianak" "Asia/Pyongyang" "Asia/Qatar"
    "Asia/Qostanay" "Asia/Qyzylorda" "Asia/Rangoon" "Asia/Riyadh"
    "Asia/Saigon" "Asia/Sakhalin" "Asia/Samarkand" "Asia/Seoul"
    "Asia/Shanghai" "Asia/Singapore" "Asia/Srednekolymsk" "Asia/Taipei"
    "Asia/Tashkent" "Asia/Tbilisi" "Asia/Tehran" "Asia/Tel_Aviv"
    "Asia/Thimbu" "Asia/Thimphu" "Asia/Tokyo" "Asia/Tomsk"
    "Asia/Ujung_Pandang" "Asia/Ulaanbaatar" "Asia/Ulan_Bator"
    "Asia/Urumqi" "Asia/Ust-Nera" "Asia/Vientiane" "Asia/Vladivostok"
    "Asia/Yakutsk" "Asia/Yangon" "Asia/Yekaterinburg" "Asia/Yerevan"
    "Atlantic/Azores" "Atlantic/Bermuda" "Atlantic/Canary"
    "Atlantic/Cape_Verde" "Atlantic/Faeroe" "Atlantic/Faroe"
    "Atlantic/Jan_Mayen" "Atlantic/Madeira" "Atlantic/Reykjavik"
    "Atlantic/South_Georgia" "Atlantic/St_Helena" "Atlantic/Stanley"
    "Australia/ACT" "Australia/Adelaide" "Australia/Brisbane"
    "Australia/Broken_Hill" "Australia/Canberra" "Australia/Currie"
    "Australia/Darwin" "Australia/Eucla" "Australia/Hobart"
    "Australia/LHI" "Australia/Lindeman" "Australia/Lord_Howe"
    "Australia/Melbourne" "Australia/NSW" "Australia/North"
    "Australia/Perth" "Australia/Queensland" "Australia/South"
    "Australia/Sydney" "Australia/Tasmania" "Australia/Victoria"
    "Australia/West" "Australia/Yancowinna" "Brazil/Acre"
    "Brazil/DeNoronha" "Brazil/East" "Brazil/West" "CET" "CST6CDT"
    "Canada/Atlantic" "Canada/Central" "Canada/Eastern"
    "Canada/Mountain" "Canada/Newfoundland" "Canada/Pacific"
    "Canada/Saskatchewan" "Canada/Yukon" "Chile/Continental"
    "Chile/EasterIsland" "Cuba" "EET" "EST" "EST5EDT" "Egypt" "Eire"
    "Etc/GMT" "Etc/GMT+0" "Etc/GMT+1" "Etc/GMT+10" "Etc/GMT+11"
    "Etc/GMT+12" "Etc/GMT+2" "Etc/GMT+3" "Etc/GMT+4" "Etc/GMT+5"
    "Etc/GMT+6" "Etc/GMT+7" "Etc/GMT+8" "Etc/GMT+9" "Etc/GMT-0"
    "Etc/GMT-1" "Etc/GMT-10" "Etc/GMT-11" "Etc/GMT-12" "Etc/GMT-13"
    "Etc/GMT-14" "Etc/GMT-2" "Etc/GMT-3" "Etc/GMT-4" "Etc/GMT-5"
    "Etc/GMT-6" "Etc/GMT-7" "Etc/GMT-8" "Etc/GMT-9" "Etc/GMT0"
    "Etc/Greenwich" "Etc/UCT" "Etc/UTC" "Etc/Universal" "Etc/Zulu"
    "Europe/Amsterdam" "Europe/Andorra" "Europe/Astrakhan"
    "Europe/Athens" "Europe/Belfast" "Europe/Belgrade" "Europe/Berlin"
    "Europe/Bratislava" "Europe/Brussels" "Europe/Bucharest"
    "Europe/Budapest" "Europe/Busingen" "Europe/Chisinau"
    "Europe/Copenhagen" "Europe/Dublin" "Europe/Gibraltar"
    "Europe/Guernsey" "Europe/Helsinki" "Europe/Isle_of_Man"
    "Europe/Istanbul" "Europe/Jersey" "Europe/Kaliningrad" "Europe/Kiev"
    "Europe/Kirov" "Europe/Lisbon" "Europe/Ljubljana" "Europe/London"
    "Europe/Luxembourg" "Europe/Madrid" "Europe/Malta"
    "Europe/Mariehamn" "Europe/Minsk" "Europe/Monaco" "Europe/Moscow"
    "Europe/Nicosia" "Europe/Oslo" "Europe/Paris" "Europe/Podgorica"
    "Europe/Prague" "Europe/Riga" "Europe/Rome" "Europe/Samara"
    "Europe/San_Marino" "Europe/Sarajevo" "Europe/Saratov"
    "Europe/Simferopol" "Europe/Skopje" "Europe/Sofia"
    "Europe/Stockholm" "Europe/Tallinn" "Europe/Tirane"
    "Europe/Tiraspol" "Europe/Ulyanovsk" "Europe/Uzhgorod"
    "Europe/Vaduz" "Europe/Vatican" "Europe/Vienna" "Europe/Vilnius"
    "Europe/Volgograd" "Europe/Warsaw" "Europe/Zagreb"
    "Europe/Zaporozhye" "Europe/Zurich" "Factory" "GB" "GB-Eire" "GMT"
    "GMT+0" "GMT-0" "GMT0" "Greenwich" "HST" "Hongkong" "Iceland"
    "Indian/Antananarivo" "Indian/Chagos" "Indian/Christmas"
    "Indian/Cocos" "Indian/Comoro" "Indian/Kerguelen" "Indian/Mahe"
    "Indian/Maldives" "Indian/Mauritius" "Indian/Mayotte"
    "Indian/Reunion" "Iran" "Israel" "Jamaica" "Japan" "Kwajalein"
    "Libya" "MET" "MST" "MST7MDT" "Mexico/BajaNorte" "Mexico/BajaSur"
    "Mexico/General" "NZ" "NZ-CHAT" "Navajo" "PRC" "PST8PDT"
    "Pacific/Apia" "Pacific/Auckland" "Pacific/Bougainville"
    "Pacific/Chatham" "Pacific/Chuuk" "Pacific/Easter" "Pacific/Efate"
    "Pacific/Enderbury" "Pacific/Fakaofo" "Pacific/Fiji"
    "Pacific/Funafuti" "Pacific/Galapagos" "Pacific/Gambier"
    "Pacific/Guadalcanal" "Pacific/Guam" "Pacific/Honolulu"
    "Pacific/Johnston" "Pacific/Kanton" "Pacific/Kiritimati"
    "Pacific/Kosrae" "Pacific/Kwajalein" "Pacific/Majuro"
    "Pacific/Marquesas" "Pacific/Midway" "Pacific/Nauru" "Pacific/Niue"
    "Pacific/Norfolk" "Pacific/Noumea" "Pacific/Pago_Pago"
    "Pacific/Palau" "Pacific/Pitcairn" "Pacific/Pohnpei"
    "Pacific/Ponape" "Pacific/Port_Moresby" "Pacific/Rarotonga"
    "Pacific/Saipan" "Pacific/Samoa" "Pacific/Tahiti" "Pacific/Tarawa"
    "Pacific/Tongatapu" "Pacific/Truk" "Pacific/Wake" "Pacific/Wallis"
    "Pacific/Yap" "Poland" "Portugal" "ROC" "ROK" "Singapore" "Turkey"
    "UCT" "US/Alaska" "US/Aleutian" "US/Arizona" "US/Central"
    "US/East-Indiana" "US/Eastern" "US/Hawaii" "US/Indiana-Starke"
    "US/Michigan" "US/Mountain" "US/Pacific" "US/Samoa" "UTC"
    "Universal" "W-SU" "WET" "Zulu")
  "List of timezones from the tzdata package.")

(provide 'biome-api-data)
;;; biome-api-data.el ends here
