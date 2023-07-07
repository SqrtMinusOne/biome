;;; biome-api-data.el --- Open Meteo API description  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Korytov Pavel

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
      ((:name . "Select Coordinates or City")
       (:fields
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Additional Variables")
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
          ("freezinglevel_height"
           (:name . "Freezinglevel Height")
           (:type . checkbox))))
        ((:name . "Solar Radiation Variables")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("terrestrial_radiation"
           (:name . "Terrestrial Solar Radiation")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox))
          ("terrestrial_radiation_instant"
           (:name . "Terrestrial Solar Radiation (Instant)")
           (:type . checkbox))))
        ((:name . "Pressure Level Variables")
         (:children
          ((:name . "Temperature")
           (:fields
            ("temperature_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("temperature_975hPa"
             (:name . "975 hPa")
             (:type . checkbox))
            ("temperature_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("temperature_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("temperature_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("temperature_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("temperature_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("temperature_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("temperature_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("temperature_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("temperature_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("temperature_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("temperature_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("temperature_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("temperature_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("temperature_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("temperature_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("temperature_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("temperature_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))))
          ((:name . "Relative Humidity")
           (:fields
            ("relativehumidity_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("relativehumidity_975hPa"
             (:name . "975 hPa")
             (:type . checkbox))
            ("relativehumidity_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("relativehumidity_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("relativehumidity_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("relativehumidity_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("relativehumidity_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("relativehumidity_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("relativehumidity_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("relativehumidity_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("relativehumidity_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("relativehumidity_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("relativehumidity_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("relativehumidity_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("relativehumidity_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("relativehumidity_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("relativehumidity_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("relativehumidity_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("relativehumidity_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))))
          ((:name . "Cloudcover")
           (:fields
            ("cloudcover_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("cloudcover_975hPa"
             (:name . "975 hPa")
             (:type . checkbox))
            ("cloudcover_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("cloudcover_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("cloudcover_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("cloudcover_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("cloudcover_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("cloudcover_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("cloudcover_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("cloudcover_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("cloudcover_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("cloudcover_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("cloudcover_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("cloudcover_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("cloudcover_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("cloudcover_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("cloudcover_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("cloudcover_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("cloudcover_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))))
          ((:name . "Wind Speed")
           (:fields
            ("windspeed_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("windspeed_975hPa"
             (:name . "975 hPa")
             (:type . checkbox))
            ("windspeed_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("windspeed_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("windspeed_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("windspeed_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("windspeed_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("windspeed_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("windspeed_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("windspeed_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("windspeed_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("windspeed_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("windspeed_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("windspeed_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("windspeed_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("windspeed_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("windspeed_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("windspeed_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("windspeed_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))))
          ((:name . "Wind Direction")
           (:fields
            ("winddirection_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("winddirection_975hPa"
             (:name . "975 hPa")
             (:type . checkbox))
            ("winddirection_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("winddirection_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("winddirection_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("winddirection_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("winddirection_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("winddirection_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("winddirection_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("winddirection_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("winddirection_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("winddirection_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("winddirection_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("winddirection_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("winddirection_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("winddirection_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("winddirection_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("winddirection_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("winddirection_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))))
          ((:name . "Geopotential Height")
           (:fields
            ("geopotential_height_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("geopotential_height_975hPa"
             (:name . "975 hPa")
             (:type . checkbox))
            ("geopotential_height_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("geopotential_height_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("geopotential_height_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("geopotential_height_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("geopotential_height_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("geopotential_height_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("geopotential_height_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("geopotential_height_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("geopotential_height_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("geopotential_height_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("geopotential_height_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("geopotential_height_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("geopotential_height_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("geopotential_height_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("geopotential_height_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("geopotential_height_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("geopotential_height_30hPa"
             (:name . "30 hPa")
             (:type . checkbox)))))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relativehumidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dewpoint_2m"
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
        ("weathercode"
         (:name . "Weathercode")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloudcover"
         (:name . "Cloudcover Total")
         (:type . checkbox))
        ("cloudcover_low"
         (:name . "Cloudcover Low")
         (:type . checkbox))
        ("cloudcover_mid"
         (:name . "Cloudcover Mid")
         (:type . checkbox))
        ("cloudcover_high"
         (:name . "Cloudcover High")
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
        ("vapor_pressure_deficit"
         (:name . "Vapor Pressure Deficit")
         (:type . checkbox))
        ("windspeed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("windspeed_80m"
         (:name . "Wind Speed (80 m)")
         (:type . checkbox))
        ("windspeed_120m"
         (:name . "Wind Speed (120 m)")
         (:type . checkbox))
        ("windspeed_180m"
         (:name . "Wind Speed (180 m)")
         (:type . checkbox))
        ("winddirection_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("winddirection_80m"
         (:name . "Wind Direction (80 m)")
         (:type . checkbox))
        ("winddirection_120m"
         (:name . "Wind Direction (120 m)")
         (:type . checkbox))
        ("winddirection_180m"
         (:name . "Wind Direction (180 m)")
         (:type . checkbox))
        ("windgusts_10m"
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
        ("soil_moisture_0_1cm"
         (:name . "Soil Moisture (0-1 cm)")
         (:type . checkbox))
        ("soil_moisture_1_3cm"
         (:name . "Soil Moisture (1-3 cm)")
         (:type . checkbox))
        ("soil_moisture_3_9cm"
         (:name . "Soil Moisture (3-9 cm)")
         (:type . checkbox))
        ("soil_moisture_9_27cm"
         (:name . "Soil Moisture (9-27 cm)")
         (:type . checkbox))
        ("soil_moisture_27_81cm"
         (:name . "Soil Moisture (27-81 cm)")
         (:type . checkbox))))
      ((:param . "daily")
       (:name . "Daily Weather Variables")
       (:fields
        ("weathercode"
         (:name . "Weathercode")
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
        ("windspeed_10m_max"
         (:name . "Maximum Wind Speed (10 m)")
         (:type . checkbox))
        ("windgusts_10m_max"
         (:name . "Maximum Wind Gusts (10 m)")
         (:type . checkbox))
        ("winddirection_10m_dominant"
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
        ("windspeed_unit"
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
          ("unixtime" . "Unix timestamp")))
        ("past_days"
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("forecast_days"
         (:type . number)
         (:min . 0)
         (:max . 16))
        ("start_date"
         (:type . date))
        ("end_date"
         (:type . date))
        ("timezone"
         (:type . timezone))))
      ((:param . "models")
       (:name . "Weather models")
       (:fields
        ("best_match"
         (:name . "Best match")
         (:type . checkbox))
        ("ecmwf_ifs04"
         (:name . "ECMWF IFS")
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
         (:name . "DWD Icon Seamless")
         (:type . checkbox))
        ("icon_global"
         (:name . "DWD Icon Global")
         (:type . checkbox))
        ("icon_eu"
         (:name . "DWD Icon EU")
         (:type . checkbox))
        ("icon_d2"
         (:name . "DWD Icon D2")
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
         (:name . "MeteoFrance Seamless")
         (:type . checkbox))
        ("meteofrance_arpege_world"
         (:name . "MeteoFrance Arpege World")
         (:type . checkbox))
        ("meteofrance_arpege_europe"
         (:name . "MeteoFrance Arpege Europe")
         (:type . checkbox))
        ("meteofrance_arome_france"
         (:name . "MeteoFrance Arome France")
         (:type . checkbox))
        ("meteofrance_arome_france_hd"
         (:name . "MeteoFrance Arome France HD")
         (:type . checkbox))))))
    ("DWD ICON"
     (:name . "DWD ICON")
     (:url . "https://open-meteo.com/en/docs/dwd-api")
     (:description . "German Weather Service ICON model. 15-minutely data for Central Europe")
     (:key . "wd")
     (:sections
      ((:name . "Select Coordinates or City")
       (:fields
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Additional Variables")
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
          ("freezinglevel_height"
           (:name . "Freezinglevel Height")
           (:type . checkbox))
          ("snowfall_height"
           (:name . "Snowfall Height (1)")
           (:type . checkbox))))
        ((:name . "Solar Radiation Variables")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("terrestrial_radiation"
           (:name . "Terrestrial Solar Radiation")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox))
          ("terrestrial_radiation_instant"
           (:name . "Terrestrial Solar Radiation (Instant)")
           (:type . checkbox))))
        ((:name . "Pressure Level Variables")
         (:children
          ((:name . "Temperature")
           (:fields
            ("temperature_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("temperature_975hPa"
             (:name . "975 hPa")
             (:type . checkbox))
            ("temperature_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("temperature_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("temperature_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("temperature_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("temperature_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("temperature_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("temperature_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("temperature_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("temperature_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("temperature_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("temperature_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("temperature_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("temperature_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("temperature_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("temperature_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("temperature_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("temperature_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))))
          ((:name . "Relative Humidity")
           (:fields
            ("relativehumidity_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("relativehumidity_975hPa"
             (:name . "975 hPa")
             (:type . checkbox))
            ("relativehumidity_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("relativehumidity_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("relativehumidity_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("relativehumidity_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("relativehumidity_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("relativehumidity_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("relativehumidity_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("relativehumidity_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("relativehumidity_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("relativehumidity_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("relativehumidity_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("relativehumidity_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("relativehumidity_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("relativehumidity_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("relativehumidity_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("relativehumidity_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("relativehumidity_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))))
          ((:name . "Cloudcover")
           (:fields
            ("cloudcover_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("cloudcover_975hPa"
             (:name . "975 hPa")
             (:type . checkbox))
            ("cloudcover_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("cloudcover_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("cloudcover_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("cloudcover_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("cloudcover_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("cloudcover_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("cloudcover_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("cloudcover_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("cloudcover_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("cloudcover_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("cloudcover_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("cloudcover_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("cloudcover_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("cloudcover_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("cloudcover_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("cloudcover_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("cloudcover_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))))
          ((:name . "Wind Speed")
           (:fields
            ("windspeed_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("windspeed_975hPa"
             (:name . "975 hPa")
             (:type . checkbox))
            ("windspeed_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("windspeed_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("windspeed_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("windspeed_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("windspeed_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("windspeed_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("windspeed_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("windspeed_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("windspeed_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("windspeed_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("windspeed_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("windspeed_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("windspeed_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("windspeed_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("windspeed_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("windspeed_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("windspeed_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))))
          ((:name . "Wind Direction")
           (:fields
            ("winddirection_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("winddirection_975hPa"
             (:name . "975 hPa")
             (:type . checkbox))
            ("winddirection_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("winddirection_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("winddirection_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("winddirection_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("winddirection_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("winddirection_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("winddirection_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("winddirection_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("winddirection_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("winddirection_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("winddirection_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("winddirection_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("winddirection_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("winddirection_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("winddirection_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("winddirection_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("winddirection_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))))
          ((:name . "Geopotential Height")
           (:fields
            ("geopotential_height_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("geopotential_height_975hPa"
             (:name . "975 hPa")
             (:type . checkbox))
            ("geopotential_height_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("geopotential_height_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("geopotential_height_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("geopotential_height_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("geopotential_height_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("geopotential_height_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("geopotential_height_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("geopotential_height_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("geopotential_height_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("geopotential_height_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("geopotential_height_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("geopotential_height_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("geopotential_height_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("geopotential_height_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("geopotential_height_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("geopotential_height_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("geopotential_height_30hPa"
             (:name . "30 hPa")
             (:type . checkbox)))))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relativehumidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dewpoint_2m"
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
        ("weathercode"
         (:name . "Weathercode")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloudcover"
         (:name . "Cloudcover Total")
         (:type . checkbox))
        ("cloudcover_low"
         (:name . "Cloudcover Low")
         (:type . checkbox))
        ("cloudcover_mid"
         (:name . "Cloudcover Mid")
         (:type . checkbox))
        ("cloudcover_high"
         (:name . "Cloudcover High")
         (:type . checkbox))
        ("evapotranspiration"
         (:name . "Evapotranspiration")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))
        ("vapor_pressure_deficit"
         (:name . "Vapor Pressure Deficit")
         (:type . checkbox))
        ("windspeed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("windspeed_80m"
         (:name . "Wind Speed (80 m)")
         (:type . checkbox))
        ("windspeed_120m"
         (:name . "Wind Speed (120 m)")
         (:type . checkbox))
        ("windspeed_180m"
         (:name . "Wind Speed (180 m)")
         (:type . checkbox))
        ("winddirection_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("winddirection_80m"
         (:name . "Wind Direction (80 m)")
         (:type . checkbox))
        ("winddirection_120m"
         (:name . "Wind Direction (120 m)")
         (:type . checkbox))
        ("winddirection_180m"
         (:name . "Wind Direction (180 m)")
         (:type . checkbox))
        ("windgusts_10m"
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
        ("soil_moisture_0_1cm"
         (:name . "Soil Moisture (0-1 cm)")
         (:type . checkbox))
        ("soil_moisture_1_3cm"
         (:name . "Soil Moisture (1-3 cm)")
         (:type . checkbox))
        ("soil_moisture_3_9cm"
         (:name . "Soil Moisture (3-9 cm)")
         (:type . checkbox))
        ("soil_moisture_9_27cm"
         (:name . "Soil Moisture (9-27 cm)")
         (:type . checkbox))
        ("soil_moisture_27_81cm"
         (:name . "Soil Moisture (27-81 cm)")
         (:type . checkbox))))
      ((:param . "minutely_15")
       (:name . "15-Minutely Weather Variables")
       (:children
        ((:name . "Solar Radiation Variables")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("terrestrial_radiation"
           (:name . "Terrestrial Solar Radiation")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox))
          ("terrestrial_radiation_instant"
           (:name . "Terrestrial Solar Radiation (Instant)")
           (:type . checkbox)))))
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
        ("freezinglevel_height"
         (:name . "Freezinglevel Height")
         (:type . checkbox))
        ("cape"
         (:name . "CAPE")
         (:type . checkbox))
        ("lightning_potential"
         (:name . "Lightning Potential Index LPI")
         (:type . checkbox))))
      ((:param . "daily")
       (:name . "Daily Weather Variables")
       (:fields
        ("weathercode"
         (:name . "Weathercode")
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
        ("windspeed_10m_max"
         (:name . "Maximum Wind Speed (10 m)")
         (:type . checkbox))
        ("windgusts_10m_max"
         (:name . "Maximum Wind Gusts (10 m)")
         (:type . checkbox))
        ("winddirection_10m_dominant"
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
        ("windspeed_unit"
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
          ("unixtime" . "Unix timestamp")))
        ("timezone"
         (:type . timezone))
        ("past_days"
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("start_date"
         (:type . date))
        ("end_date"
         (:type . date))))))
    ("NOAA GFS & HRRR"
     (:name . "NOAA GFS & HRRR")
     (:url . "https://open-meteo.com/en/docs/gfs-api")
     (:description . "Forecasts tailored for the US region")
     (:key . "wu")
     (:sections
      ((:name . "Select Coordinates or City")
       (:fields
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Additional Variables")
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
          ("lifted_index"
           (:name . "Lifted Index")
           (:type . checkbox))
          ("freezinglevel_height"
           (:name . "Freezinglevel Height")
           (:type . checkbox))))
        ((:name . "Solar Radiation Variables")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("terrestrial_radiation"
           (:name . "Terrestrial Solar Radiation")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox))
          ("terrestrial_radiation_instant"
           (:name . "Terrestrial Solar Radiation (Instant)")
           (:type . checkbox))))
        ((:name . "Pressure Level Variables")
         (:children
          ((:name . "Temperature")
           (:fields
            ("temperature_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("temperature_975hPa"
             (:name . "975 hPa")
             (:type . checkbox))
            ("temperature_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("temperature_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("temperature_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("temperature_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("temperature_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("temperature_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("temperature_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("temperature_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("temperature_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("temperature_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("temperature_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("temperature_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("temperature_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("temperature_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("temperature_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("temperature_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("temperature_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("temperature_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("temperature_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("temperature_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("temperature_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("temperature_40hPa"
             (:name . "40 hPa")
             (:type . checkbox))
            ("temperature_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("temperature_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("temperature_15hPa"
             (:name . "15 hPa")
             (:type . checkbox))
            ("temperature_10hPa"
             (:name . "10 hPa")
             (:type . checkbox))))
          ((:name . "Dewpoint")
           (:fields
            ("dewpoint_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("dewpoint_975hPa"
             (:name . "975 hPa")
             (:type . checkbox))
            ("dewpoint_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("dewpoint_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("dewpoint_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("dewpoint_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("dewpoint_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("dewpoint_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("dewpoint_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("dewpoint_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("dewpoint_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("dewpoint_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("dewpoint_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("dewpoint_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("dewpoint_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("dewpoint_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("dewpoint_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("dewpoint_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("dewpoint_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("dewpoint_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("dewpoint_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("dewpoint_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("dewpoint_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("dewpoint_40hPa"
             (:name . "40 hPa")
             (:type . checkbox))
            ("dewpoint_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("dewpoint_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("dewpoint_15hPa"
             (:name . "15 hPa")
             (:type . checkbox))
            ("dewpoint_10hPa"
             (:name . "10 hPa")
             (:type . checkbox))))
          ((:name . "Relative Humidity")
           (:fields
            ("relativehumidity_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("relativehumidity_975hPa"
             (:name . "975 hPa")
             (:type . checkbox))
            ("relativehumidity_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("relativehumidity_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("relativehumidity_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("relativehumidity_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("relativehumidity_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("relativehumidity_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("relativehumidity_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("relativehumidity_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("relativehumidity_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("relativehumidity_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("relativehumidity_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("relativehumidity_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("relativehumidity_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("relativehumidity_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("relativehumidity_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("relativehumidity_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("relativehumidity_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("relativehumidity_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("relativehumidity_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("relativehumidity_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("relativehumidity_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("relativehumidity_40hPa"
             (:name . "40 hPa")
             (:type . checkbox))
            ("relativehumidity_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("relativehumidity_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("relativehumidity_15hPa"
             (:name . "15 hPa")
             (:type . checkbox))
            ("relativehumidity_10hPa"
             (:name . "10 hPa")
             (:type . checkbox))))
          ((:name . "Cloudcover")
           (:fields
            ("cloudcover_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("cloudcover_975hPa"
             (:name . "975 hPa")
             (:type . checkbox))
            ("cloudcover_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("cloudcover_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("cloudcover_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("cloudcover_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("cloudcover_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("cloudcover_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("cloudcover_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("cloudcover_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("cloudcover_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("cloudcover_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("cloudcover_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("cloudcover_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("cloudcover_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("cloudcover_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("cloudcover_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("cloudcover_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("cloudcover_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("cloudcover_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("cloudcover_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("cloudcover_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("cloudcover_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("cloudcover_40hPa"
             (:name . "40 hPa")
             (:type . checkbox))
            ("cloudcover_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("cloudcover_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("cloudcover_15hPa"
             (:name . "15 hPa")
             (:type . checkbox))
            ("cloudcover_10hPa"
             (:name . "10 hPa")
             (:type . checkbox))))
          ((:name . "Wind Speed")
           (:fields
            ("windspeed_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("windspeed_975hPa"
             (:name . "975 hPa")
             (:type . checkbox))
            ("windspeed_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("windspeed_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("windspeed_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("windspeed_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("windspeed_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("windspeed_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("windspeed_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("windspeed_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("windspeed_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("windspeed_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("windspeed_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("windspeed_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("windspeed_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("windspeed_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("windspeed_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("windspeed_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("windspeed_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("windspeed_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("windspeed_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("windspeed_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("windspeed_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("windspeed_40hPa"
             (:name . "40 hPa")
             (:type . checkbox))
            ("windspeed_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("windspeed_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("windspeed_15hPa"
             (:name . "15 hPa")
             (:type . checkbox))
            ("windspeed_10hPa"
             (:name . "10 hPa")
             (:type . checkbox))))
          ((:name . "Wind Direction")
           (:fields
            ("winddirection_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("winddirection_975hPa"
             (:name . "975 hPa")
             (:type . checkbox))
            ("winddirection_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("winddirection_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("winddirection_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("winddirection_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("winddirection_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("winddirection_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("winddirection_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("winddirection_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("winddirection_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("winddirection_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("winddirection_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("winddirection_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("winddirection_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("winddirection_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("winddirection_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("winddirection_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("winddirection_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("winddirection_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("winddirection_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("winddirection_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("winddirection_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("winddirection_40hPa"
             (:name . "40 hPa")
             (:type . checkbox))
            ("winddirection_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("winddirection_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("winddirection_15hPa"
             (:name . "15 hPa")
             (:type . checkbox))
            ("winddirection_10hPa"
             (:name . "10 hPa")
             (:type . checkbox))))
          ((:name . "Vertical Velocity")
           (:fields
            ("vertical_velocity_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("vertical_velocity_975hPa"
             (:name . "975 hPa")
             (:type . checkbox))
            ("vertical_velocity_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("vertical_velocity_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("vertical_velocity_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("vertical_velocity_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("vertical_velocity_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("vertical_velocity_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("vertical_velocity_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("vertical_velocity_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("vertical_velocity_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("vertical_velocity_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("vertical_velocity_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("vertical_velocity_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("vertical_velocity_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("vertical_velocity_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("vertical_velocity_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("vertical_velocity_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("vertical_velocity_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("vertical_velocity_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("vertical_velocity_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("vertical_velocity_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("vertical_velocity_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("vertical_velocity_40hPa"
             (:name . "40 hPa")
             (:type . checkbox))
            ("vertical_velocity_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("vertical_velocity_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("vertical_velocity_15hPa"
             (:name . "15 hPa")
             (:type . checkbox))
            ("vertical_velocity_10hPa"
             (:name . "10 hPa")
             (:type . checkbox))))
          ((:name . "Geopotential Height")
           (:fields
            ("geopotential_height_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("geopotential_height_975hPa"
             (:name . "975 hPa")
             (:type . checkbox))
            ("geopotential_height_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("geopotential_height_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("geopotential_height_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("geopotential_height_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("geopotential_height_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("geopotential_height_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("geopotential_height_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("geopotential_height_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("geopotential_height_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("geopotential_height_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("geopotential_height_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("geopotential_height_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("geopotential_height_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("geopotential_height_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("geopotential_height_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("geopotential_height_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("geopotential_height_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("geopotential_height_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("geopotential_height_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("geopotential_height_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("geopotential_height_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("geopotential_height_40hPa"
             (:name . "40 hPa")
             (:type . checkbox))
            ("geopotential_height_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("geopotential_height_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("geopotential_height_15hPa"
             (:name . "15 hPa")
             (:type . checkbox))
            ("geopotential_height_10hPa"
             (:name . "10 hPa")
             (:type . checkbox)))))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relativehumidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dewpoint_2m"
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
        ("weathercode"
         (:name . "Weathercode")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloudcover"
         (:name . "Cloudcover Total")
         (:type . checkbox))
        ("cloudcover_low"
         (:name . "Cloudcover Low")
         (:type . checkbox))
        ("cloudcover_mid"
         (:name . "Cloudcover Mid")
         (:type . checkbox))
        ("cloudcover_high"
         (:name . "Cloudcover High")
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
        ("vapor_pressure_deficit"
         (:name . "Vapor Pressure Deficit")
         (:type . checkbox))
        ("windspeed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("windspeed_80m"
         (:name . "Wind Speed (80 m)")
         (:type . checkbox))
        ("winddirection_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("winddirection_80m"
         (:name . "Wind Direction (80 m)")
         (:type . checkbox))
        ("windgusts_10m"
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
        ("weathercode"
         (:name . "Weathercode")
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
        ("windspeed_10m_max"
         (:name . "Maximum Wind Speed (10 m)")
         (:type . checkbox))
        ("windgusts_10m_max"
         (:name . "Maximum Wind Gusts (10 m)")
         (:type . checkbox))
        ("winddirection_10m_dominant"
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
        ("windspeed_unit"
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
          ("unixtime" . "Unix timestamp")))
        ("past_days"
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("forecast_days"
         (:type . number)
         (:min . 0)
         (:max . 16))
        ("start_date"
         (:type . date))
        ("end_date"
         (:type . date))
        ("timezone"
         (:type . timezone))))))
    ("MeteoFrance"
     (:name . "MeteoFrance")
     (:url . "https://open-meteo.com/en/docs/meteofrance-api")
     (:description . "Forecasts tailored for Central Europe and France")
     (:key . "wf")
     (:sections
      ((:name . "Select Coordinates or City")
       (:fields
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Additional Variables")
         (:fields
          ("is_day"
           (:name . "Is Day or Night")
           (:type . checkbox))
          ("cape"
           (:name . "CAPE")
           (:type . checkbox))))
        ((:name . "Solar Radiation Variables")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("terrestrial_radiation"
           (:name . "Terrestrial Solar Radiation")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox))
          ("terrestrial_radiation_instant"
           (:name . "Terrestrial Solar Radiation (Instant)")
           (:type . checkbox))))
        ((:name . "Pressure Level Variables")
         (:children
          ((:name . "Temperature")
           (:fields
            ("temperature_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("temperature_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("temperature_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("temperature_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("temperature_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("temperature_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("temperature_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("temperature_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("temperature_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("temperature_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("temperature_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("temperature_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("temperature_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("temperature_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("temperature_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("temperature_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("temperature_275hPa"
             (:name . "275 hPa")
             (:type . checkbox))
            ("temperature_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("temperature_225hPa"
             (:name . "225 hPa")
             (:type . checkbox))
            ("temperature_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("temperature_175hPa"
             (:name . "175 hPa")
             (:type . checkbox))
            ("temperature_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("temperature_125hPa"
             (:name . "125 hPa")
             (:type . checkbox))
            ("temperature_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("temperature_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("temperature_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("temperature_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("temperature_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("temperature_10hPa"
             (:name . "10 hPa")
             (:type . checkbox))))
          ((:name . "Dewpoint")
           (:fields
            ("dewpoint_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("dewpoint_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("dewpoint_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("dewpoint_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("dewpoint_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("dewpoint_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("dewpoint_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("dewpoint_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("dewpoint_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("dewpoint_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("dewpoint_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("dewpoint_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("dewpoint_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("dewpoint_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("dewpoint_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("dewpoint_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("dewpoint_275hPa"
             (:name . "275 hPa")
             (:type . checkbox))
            ("dewpoint_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("dewpoint_225hPa"
             (:name . "225 hPa")
             (:type . checkbox))
            ("dewpoint_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("dewpoint_175hPa"
             (:name . "175 hPa")
             (:type . checkbox))
            ("dewpoint_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("dewpoint_125hPa"
             (:name . "125 hPa")
             (:type . checkbox))
            ("dewpoint_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("dewpoint_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("dewpoint_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("dewpoint_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("dewpoint_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("dewpoint_10hPa"
             (:name . "10 hPa")
             (:type . checkbox))))
          ((:name . "Relative Humidity")
           (:fields
            ("relativehumidity_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("relativehumidity_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("relativehumidity_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("relativehumidity_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("relativehumidity_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("relativehumidity_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("relativehumidity_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("relativehumidity_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("relativehumidity_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("relativehumidity_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("relativehumidity_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("relativehumidity_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("relativehumidity_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("relativehumidity_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("relativehumidity_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("relativehumidity_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("relativehumidity_275hPa"
             (:name . "275 hPa")
             (:type . checkbox))
            ("relativehumidity_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("relativehumidity_225hPa"
             (:name . "225 hPa")
             (:type . checkbox))
            ("relativehumidity_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("relativehumidity_175hPa"
             (:name . "175 hPa")
             (:type . checkbox))
            ("relativehumidity_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("relativehumidity_125hPa"
             (:name . "125 hPa")
             (:type . checkbox))
            ("relativehumidity_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("relativehumidity_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("relativehumidity_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("relativehumidity_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("relativehumidity_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("relativehumidity_10hPa"
             (:name . "10 hPa")
             (:type . checkbox))))
          ((:name . "Cloudcover")
           (:fields
            ("cloudcover_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("cloudcover_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("cloudcover_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("cloudcover_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("cloudcover_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("cloudcover_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("cloudcover_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("cloudcover_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("cloudcover_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("cloudcover_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("cloudcover_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("cloudcover_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("cloudcover_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("cloudcover_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("cloudcover_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("cloudcover_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("cloudcover_275hPa"
             (:name . "275 hPa")
             (:type . checkbox))
            ("cloudcover_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("cloudcover_225hPa"
             (:name . "225 hPa")
             (:type . checkbox))
            ("cloudcover_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("cloudcover_175hPa"
             (:name . "175 hPa")
             (:type . checkbox))
            ("cloudcover_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("cloudcover_125hPa"
             (:name . "125 hPa")
             (:type . checkbox))
            ("cloudcover_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("cloudcover_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("cloudcover_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("cloudcover_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("cloudcover_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("cloudcover_10hPa"
             (:name . "10 hPa")
             (:type . checkbox))))
          ((:name . "Wind Speed")
           (:fields
            ("windspeed_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("windspeed_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("windspeed_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("windspeed_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("windspeed_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("windspeed_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("windspeed_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("windspeed_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("windspeed_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("windspeed_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("windspeed_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("windspeed_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("windspeed_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("windspeed_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("windspeed_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("windspeed_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("windspeed_275hPa"
             (:name . "275 hPa")
             (:type . checkbox))
            ("windspeed_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("windspeed_225hPa"
             (:name . "225 hPa")
             (:type . checkbox))
            ("windspeed_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("windspeed_175hPa"
             (:name . "175 hPa")
             (:type . checkbox))
            ("windspeed_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("windspeed_125hPa"
             (:name . "125 hPa")
             (:type . checkbox))
            ("windspeed_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("windspeed_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("windspeed_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("windspeed_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("windspeed_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("windspeed_10hPa"
             (:name . "10 hPa")
             (:type . checkbox))))
          ((:name . "Wind Direction")
           (:fields
            ("winddirection_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("winddirection_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("winddirection_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("winddirection_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("winddirection_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("winddirection_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("winddirection_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("winddirection_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("winddirection_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("winddirection_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("winddirection_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("winddirection_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("winddirection_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("winddirection_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("winddirection_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("winddirection_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("winddirection_275hPa"
             (:name . "275 hPa")
             (:type . checkbox))
            ("winddirection_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("winddirection_225hPa"
             (:name . "225 hPa")
             (:type . checkbox))
            ("winddirection_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("winddirection_175hPa"
             (:name . "175 hPa")
             (:type . checkbox))
            ("winddirection_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("winddirection_125hPa"
             (:name . "125 hPa")
             (:type . checkbox))
            ("winddirection_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("winddirection_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("winddirection_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("winddirection_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("winddirection_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("winddirection_10hPa"
             (:name . "10 hPa")
             (:type . checkbox))))
          ((:name . "Geopotential Height")
           (:fields
            ("geopotential_height_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("geopotential_height_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("geopotential_height_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("geopotential_height_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("geopotential_height_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("geopotential_height_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("geopotential_height_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("geopotential_height_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("geopotential_height_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("geopotential_height_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("geopotential_height_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("geopotential_height_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("geopotential_height_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("geopotential_height_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("geopotential_height_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("geopotential_height_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("geopotential_height_275hPa"
             (:name . "275 hPa")
             (:type . checkbox))
            ("geopotential_height_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("geopotential_height_225hPa"
             (:name . "225 hPa")
             (:type . checkbox))
            ("geopotential_height_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("geopotential_height_175hPa"
             (:name . "175 hPa")
             (:type . checkbox))
            ("geopotential_height_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("geopotential_height_125hPa"
             (:name . "125 hPa")
             (:type . checkbox))
            ("geopotential_height_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("geopotential_height_70hPa"
             (:name . "70 hPa")
             (:type . checkbox))
            ("geopotential_height_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("geopotential_height_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("geopotential_height_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("geopotential_height_10hPa"
             (:name . "10 hPa")
             (:type . checkbox)))))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relativehumidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dewpoint_2m"
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
        ("weathercode"
         (:name . "Weathercode")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloudcover"
         (:name . "Cloudcover Total")
         (:type . checkbox))
        ("cloudcover_low"
         (:name . "Cloudcover Low")
         (:type . checkbox))
        ("cloudcover_mid"
         (:name . "Cloudcover Mid")
         (:type . checkbox))
        ("cloudcover_high"
         (:name . "Cloudcover High")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))
        ("vapor_pressure_deficit"
         (:name . "Vapor Pressure Deficit")
         (:type . checkbox))
        ("windspeed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("windspeed_20m"
         (:name . "Wind Speed (20 m)")
         (:type . checkbox))
        ("windspeed_50m"
         (:name . "Wind Speed (50 m)")
         (:type . checkbox))
        ("windspeed_100m"
         (:name . "Wind Speed (100 m)")
         (:type . checkbox))
        ("windspeed_150m"
         (:name . "Wind Speed (150 m)")
         (:type . checkbox))
        ("windspeed_200m"
         (:name . "Wind Speed (200 m)")
         (:type . checkbox))
        ("winddirection_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("winddirection_20m"
         (:name . "Wind Direction (20 m)")
         (:type . checkbox))
        ("winddirection_50m"
         (:name . "Wind Direction (50 m)")
         (:type . checkbox))
        ("winddirection_100m"
         (:name . "Wind Direction (100 m)")
         (:type . checkbox))
        ("winddirection_150m"
         (:name . "Wind Direction (150 m)")
         (:type . checkbox))
        ("winddirection_200m"
         (:name . "Wind Direction (200 m)")
         (:type . checkbox))
        ("windgusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))
        ("temperature_20m"
         (:name . "Temperature (20 m)")
         (:type . checkbox))
        ("temperature_50m"
         (:name . "Temperature (50 m)")
         (:type . checkbox))
        ("temperature_100m"
         (:type . checkbox))
        ("temperature_100m"
         (:name . "Temperature (100 m)")
         (:name . "Temperature (150 m)")
         (:type . checkbox))
        ("temperature_200m"
         (:name . "Temperature (200 m)")
         (:type . checkbox))))
      ((:param . "daily")
       (:name . "Daily Weather Variables")
       (:fields
        ("weathercode"
         (:name . "Weathercode")
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
        ("windspeed_10m_max"
         (:name . "Maximum Wind Speed (10 m)")
         (:type . checkbox))
        ("windgusts_10m_max"
         (:name . "Maximum Wind Gusts (10 m)")
         (:type . checkbox))
        ("winddirection_10m_dominant"
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
        ("windspeed_unit"
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
          ("unixtime" . "Unix timestamp")))
        ("timezone"
         (:type . timezone))
        ("past_days"
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("start_date"
         (:type . date))
        ("end_date"
         (:type . date))))
      ((:param . "models")
       (:name . "Weather models")
       (:fields
        ("best_match"
         (:name . "Best match")
         (:type . checkbox))
        ("arpege_seamless"
         (:name . "Arpege Seamless")
         (:type . checkbox))
        ("arpege_world"
         (:name . "Arpege World")
         (:type . checkbox))
        ("arpege_europe"
         (:name . "Arpege Europe")
         (:type . checkbox))
        ("arome_seamless"
         (:name . "Arome Seamless")
         (:type . checkbox))
        ("arome_france"
         (:name . "Arome France")
         (:type . checkbox))
        ("arome_france_hd"
         (:name . "Arome France HD")
         (:type . checkbox))))))
    ("ECMWF"
     (:name . "ECMWF")
     (:url . "https://open-meteo.com/en/docs/ecmwf-api")
     (:description . "Open-data forecasts by ECMWF")
     (:key . "we")
     (:sections
      ((:name . "Select Coordinates or City")
       (:fields
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))))
      ((:param . "hourly")
       (:name . "3-Hourly Weather Variables")
       (:children
        ((:name . "Pressure Level Variables")
         (:children
          ((:name . "Temperature")
           (:fields
            ("temperature_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("temperature_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("temperature_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("temperature_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("temperature_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("temperature_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("temperature_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("temperature_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("temperature_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))))
          ((:name . "Relative Humidity")
           (:fields
            ("relative_humidity_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("relative_humidity_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("relative_humidity_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("relative_humidity_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("relative_humidity_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("relative_humidity_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("relative_humidity_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("relative_humidity_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("relative_humidity_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))))
          ((:name . "Specific Humidity")
           (:fields
            ("specific_humidity_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("specific_humidity_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("specific_humidity_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("specific_humidity_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("specific_humidity_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("specific_humidity_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("specific_humidity_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("specific_humidity_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("specific_humidity_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))))
          ((:name . "Cloudcover")
           (:fields
            ("cloudcover_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("cloudcover_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("cloudcover_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("cloudcover_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("cloudcover_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("cloudcover_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("cloudcover_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("cloudcover_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("cloudcover_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))))
          ((:name . "Wind Speed")
           (:fields
            ("windspeed_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("windspeed_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("windspeed_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("windspeed_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("windspeed_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("windspeed_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("windspeed_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("windspeed_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("windspeed_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))))
          ((:name . "Wind Direction")
           (:fields
            ("winddirection_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("winddirection_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("winddirection_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("winddirection_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("winddirection_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("winddirection_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("winddirection_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("winddirection_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("winddirection_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))))
          ((:name . "Geopotential Height")
           (:fields
            ("geopotential_height_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("geopotential_height_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("geopotential_height_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("geopotential_height_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("geopotential_height_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("geopotential_height_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("geopotential_height_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("geopotential_height_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("geopotential_height_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))))
          ((:name . "Relative Vorticity")
           (:fields
            ("atmosphere_relative_vorticity_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("atmosphere_relative_vorticity_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("atmosphere_relative_vorticity_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("atmosphere_relative_vorticity_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("atmosphere_relative_vorticity_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("atmosphere_relative_vorticity_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("atmosphere_relative_vorticity_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("atmosphere_relative_vorticity_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("atmosphere_relative_vorticity_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))))
          ((:name . "Divergence of Wind")
           (:fields
            ("divergence_of_wind_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("divergence_of_wind_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("divergence_of_wind_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("divergence_of_wind_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("divergence_of_wind_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("divergence_of_wind_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("divergence_of_wind_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("divergence_of_wind_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("divergence_of_wind_50hPa"
             (:name . "50 hPa")
             (:type . checkbox)))))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relativehumidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dewpoint_2m"
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
        ("weathercode"
         (:name . "Weathercode")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloudcover"
         (:name . "Cloudcover Total")
         (:type . checkbox))
        ("cloudcover_low"
         (:name . "Cloudcover Low")
         (:type . checkbox))
        ("cloudcover_mid"
         (:name . "Cloudcover Mid")
         (:type . checkbox))
        ("cloudcover_high"
         (:name . "Cloudcover High")
         (:type . checkbox))
        ("vapor_pressure_deficit"
         (:name . "Vapor Pressure Deficit")
         (:type . checkbox))
        ("windspeed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("winddirection_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("windgusts_10m"
         (:name . "Wind Gusts (10 m)")
         (:type . checkbox))
        ("surface_temperature"
         (:name . "Surface temperature")
         (:type . checkbox))
        ("soil_temperature_0_to_7_cm"
         (:name . "Soil Temperature (0-7 cm)")
         (:type . checkbox))
        ("runoff"
         (:name . "Surface Water Runoff")
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
        ("windspeed_unit"
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
          ("unixtime" . "Unix timestamp")))
        ("past_days"
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("start_date"
         (:type . date))
        ("end_date"
         (:type . date))))))
    ("JMA"
     (:name . "JMA")
     (:url . "https://open-meteo.com/en/docs/jma-api")
     (:description . "Forecasts tailored for Japan")
     (:key . "wj")
     (:sections
      ((:name . "Select Coordinates or City")
       (:fields
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Additional Variables")
         (:fields
          ("is_day"
           (:name . "Is Day or Night")
           (:type . checkbox))))
        ((:name . "Solar Radiation Variables (Only available for Japan)")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("terrestrial_radiation"
           (:name . "Terrestrial Solar Radiation")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox))
          ("terrestrial_radiation_instant"
           (:name . "Terrestrial Solar Radiation (Instant)")
           (:type . checkbox))))
        ((:name . "Pressure Level Variables")
         (:children
          ((:name . "Temperature")
           (:fields
            ("temperature_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("temperature_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("temperature_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("temperature_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("temperature_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("temperature_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("temperature_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("temperature_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("temperature_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("temperature_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("temperature_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))))
          ((:name . "Dewpoint")
           (:fields
            ("dewpoint_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("dewpoint_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("dewpoint_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("dewpoint_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("dewpoint_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("dewpoint_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("dewpoint_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("dewpoint_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("dewpoint_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("dewpoint_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("dewpoint_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))))
          ((:name . "Relative Humidity")
           (:fields
            ("relativehumidity_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("relativehumidity_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("relativehumidity_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("relativehumidity_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("relativehumidity_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("relativehumidity_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("relativehumidity_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("relativehumidity_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("relativehumidity_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("relativehumidity_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("relativehumidity_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))))
          ((:name . "Cloudcover")
           (:fields
            ("cloudcover_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("cloudcover_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("cloudcover_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("cloudcover_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("cloudcover_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("cloudcover_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("cloudcover_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("cloudcover_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("cloudcover_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("cloudcover_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("cloudcover_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))))
          ((:name . "Wind Speed")
           (:fields
            ("windspeed_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("windspeed_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("windspeed_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("windspeed_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("windspeed_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("windspeed_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("windspeed_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("windspeed_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("windspeed_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("windspeed_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("windspeed_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))))
          ((:name . "Wind Direction")
           (:fields
            ("winddirection_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("winddirection_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("winddirection_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("winddirection_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("winddirection_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("winddirection_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("winddirection_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("winddirection_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("winddirection_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("winddirection_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("winddirection_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))))
          ((:name . "Geopotential Height")
           (:fields
            ("geopotential_height_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("geopotential_height_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("geopotential_height_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("geopotential_height_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("geopotential_height_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("geopotential_height_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("geopotential_height_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("geopotential_height_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("geopotential_height_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("geopotential_height_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("geopotential_height_100hPa"
             (:name . "100 hPa")
             (:type . checkbox)))))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relativehumidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dewpoint_2m"
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
        ("weathercode"
         (:name . "Weathercode")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloudcover"
         (:name . "Cloudcover Total")
         (:type . checkbox))
        ("cloudcover_low"
         (:name . "Cloudcover Low")
         (:type . checkbox))
        ("cloudcover_mid"
         (:name . "Cloudcover Mid")
         (:type . checkbox))
        ("cloudcover_high"
         (:name . "Cloudcover High")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))
        ("vapor_pressure_deficit"
         (:name . "Vapor Pressure Deficit")
         (:type . checkbox))
        ("windspeed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("winddirection_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))))
      ((:param . "daily")
       (:name . "Daily Weather Variables")
       (:fields
        ("weathercode"
         (:name . "Weathercode")
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
        ("windspeed_10m_max"
         (:name . "Maximum Wind Speed (10 m)")
         (:type . checkbox))
        ("windgusts_10m_max"
         (:name . "Maximum Wind Gusts (10 m)")
         (:type . checkbox))
        ("winddirection_10m_dominant"
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
        ("windspeed_unit"
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
          ("unixtime" . "Unix timestamp")))
        ("timezone"
         (:type . timezone))
        ("past_days"
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("start_date"
         (:type . date))
        ("end_date"
         (:type . date))))))
    ("MET Norway"
     (:name . "MET Norway")
     (:url . "https://open-meteo.com/en/docs/metno-api")
     (:description . "Forecasts exclusively for North Europe")
     (:key . "wn")
     (:sections
      ((:name . "Select Coordinates or City")
       (:fields
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Additional Variables")
         (:fields
          ("is_day"
           (:name . "Is Day or Night")
           (:type . checkbox))))
        ((:name . "Solar Radiation Variables")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("terrestrial_radiation"
           (:name . "Terrestrial Solar Radiation")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox))
          ("terrestrial_radiation_instant"
           (:name . "Terrestrial Solar Radiation (Instant)")
           (:type . checkbox)))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relativehumidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dewpoint_2m"
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
        ("weathercode"
         (:name . "Weathercode")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloudcover"
         (:name . "Cloudcover Total")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))
        ("vapor_pressure_deficit"
         (:name . "Vapor Pressure Deficit")
         (:type . checkbox))
        ("windspeed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("winddirection_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("windgusts_10m"
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
        ("windspeed_unit"
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
          ("unixtime" . "Unix timestamp")))
        ("timezone"
         (:type . timezone))
        ("past_days"
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("start_date"
         (:type . date))
        ("end_date"
         (:type . date))))))
    ("GEM"
     (:name . "GEM")
     (:url . "https://open-meteo.com/en/docs/gem-api")
     (:description . "Forecasts tailored for North America")
     (:key . "wa")
     (:sections
      ((:name . "Select Coordinates or City")
       (:fields
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Additional Variables")
         (:fields
          ("is_day"
           (:name . "Is Day or Night")
           (:type . checkbox))
          ("cape"
           (:name . "CAPE")
           (:type . checkbox))))
        ((:name . "Solar Radiation Variables")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("terrestrial_radiation"
           (:name . "Terrestrial Solar Radiation")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox))
          ("terrestrial_radiation_instant"
           (:name . "Terrestrial Solar Radiation (Instant)")
           (:type . checkbox))))
        ((:name . "Pressure Level Variables")
         (:children
          ((:name . "Temperature")
           (:fields
            ("temperature_10hPa"
             (:name . "10 hPa")
             (:type . checkbox))
            ("temperature_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("temperature_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("temperature_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("temperature_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("temperature_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("temperature_175hPa"
             (:name . "175 hPa")
             (:type . checkbox))
            ("temperature_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("temperature_225hPa"
             (:name . "225 hPa")
             (:type . checkbox))
            ("temperature_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("temperature_275hPa"
             (:name . "275 hPa")
             (:type . checkbox))
            ("temperature_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("temperature_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("temperature_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("temperature_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("temperature_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("temperature_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("temperature_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("temperature_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("temperature_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("temperature_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("temperature_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("temperature_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("temperature_875hPa"
             (:name . "875 hPa")
             (:type . checkbox))
            ("temperature_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("temperature_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("temperature_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("temperature_970hPa"
             (:name . "970 hPa")
             (:type . checkbox))
            ("temperature_985hPa"
             (:name . "985 hPa")
             (:type . checkbox))
            ("temperature_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("temperature_1015hPa"
             (:name . "1015 hPa")
             (:type . checkbox))))
          ((:name . "Dewpoint")
           (:fields
            ("dewpoint_10hPa"
             (:name . "10 hPa")
             (:type . checkbox))
            ("dewpoint_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("dewpoint_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("dewpoint_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("dewpoint_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("dewpoint_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("dewpoint_175hPa"
             (:name . "175 hPa")
             (:type . checkbox))
            ("dewpoint_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("dewpoint_225hPa"
             (:name . "225 hPa")
             (:type . checkbox))
            ("dewpoint_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("dewpoint_275hPa"
             (:name . "275 hPa")
             (:type . checkbox))
            ("dewpoint_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("dewpoint_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("dewpoint_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("dewpoint_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("dewpoint_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("dewpoint_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("dewpoint_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("dewpoint_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("dewpoint_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("dewpoint_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("dewpoint_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("dewpoint_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("dewpoint_875hPa"
             (:name . "875 hPa")
             (:type . checkbox))
            ("dewpoint_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("dewpoint_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("dewpoint_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("dewpoint_970hPa"
             (:name . "970 hPa")
             (:type . checkbox))
            ("dewpoint_985hPa"
             (:name . "985 hPa")
             (:type . checkbox))
            ("dewpoint_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("dewpoint_1015hPa"
             (:name . "1015 hPa")
             (:type . checkbox))))
          ((:name . "Relative Humidity")
           (:fields
            ("relativehumidity_10hPa"
             (:name . "10 hPa")
             (:type . checkbox))
            ("relativehumidity_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("relativehumidity_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("relativehumidity_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("relativehumidity_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("relativehumidity_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("relativehumidity_175hPa"
             (:name . "175 hPa")
             (:type . checkbox))
            ("relativehumidity_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("relativehumidity_225hPa"
             (:name . "225 hPa")
             (:type . checkbox))
            ("relativehumidity_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("relativehumidity_275hPa"
             (:name . "275 hPa")
             (:type . checkbox))
            ("relativehumidity_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("relativehumidity_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("relativehumidity_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("relativehumidity_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("relativehumidity_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("relativehumidity_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("relativehumidity_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("relativehumidity_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("relativehumidity_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("relativehumidity_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("relativehumidity_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("relativehumidity_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("relativehumidity_875hPa"
             (:name . "875 hPa")
             (:type . checkbox))
            ("relativehumidity_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("relativehumidity_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("relativehumidity_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("relativehumidity_970hPa"
             (:name . "970 hPa")
             (:type . checkbox))
            ("relativehumidity_985hPa"
             (:name . "985 hPa")
             (:type . checkbox))
            ("relativehumidity_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("relativehumidity_1015hPa"
             (:name . "1015 hPa")
             (:type . checkbox))))
          ((:name . "Cloudcover")
           (:fields
            ("cloudcover_10hPa"
             (:name . "10 hPa")
             (:type . checkbox))
            ("cloudcover_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("cloudcover_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("cloudcover_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("cloudcover_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("cloudcover_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("cloudcover_175hPa"
             (:name . "175 hPa")
             (:type . checkbox))
            ("cloudcover_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("cloudcover_225hPa"
             (:name . "225 hPa")
             (:type . checkbox))
            ("cloudcover_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("cloudcover_275hPa"
             (:name . "275 hPa")
             (:type . checkbox))
            ("cloudcover_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("cloudcover_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("cloudcover_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("cloudcover_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("cloudcover_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("cloudcover_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("cloudcover_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("cloudcover_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("cloudcover_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("cloudcover_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("cloudcover_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("cloudcover_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("cloudcover_875hPa"
             (:name . "875 hPa")
             (:type . checkbox))
            ("cloudcover_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("cloudcover_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("cloudcover_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("cloudcover_970hPa"
             (:name . "970 hPa")
             (:type . checkbox))
            ("cloudcover_985hPa"
             (:name . "985 hPa")
             (:type . checkbox))
            ("cloudcover_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("cloudcover_1015hPa"
             (:name . "1015 hPa")
             (:type . checkbox))))
          ((:name . "Wind Speed")
           (:fields
            ("windspeed_10hPa"
             (:name . "10 hPa")
             (:type . checkbox))
            ("windspeed_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("windspeed_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("windspeed_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("windspeed_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("windspeed_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("windspeed_175hPa"
             (:name . "175 hPa")
             (:type . checkbox))
            ("windspeed_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("windspeed_225hPa"
             (:name . "225 hPa")
             (:type . checkbox))
            ("windspeed_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("windspeed_275hPa"
             (:name . "275 hPa")
             (:type . checkbox))
            ("windspeed_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("windspeed_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("windspeed_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("windspeed_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("windspeed_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("windspeed_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("windspeed_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("windspeed_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("windspeed_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("windspeed_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("windspeed_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("windspeed_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("windspeed_875hPa"
             (:name . "875 hPa")
             (:type . checkbox))
            ("windspeed_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("windspeed_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("windspeed_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("windspeed_970hPa"
             (:name . "970 hPa")
             (:type . checkbox))
            ("windspeed_985hPa"
             (:name . "985 hPa")
             (:type . checkbox))
            ("windspeed_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("windspeed_1015hPa"
             (:name . "1015 hPa")
             (:type . checkbox))))
          ((:name . "Wind Direction")
           (:fields
            ("winddirection_10hPa"
             (:name . "10 hPa")
             (:type . checkbox))
            ("winddirection_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("winddirection_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("winddirection_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("winddirection_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("winddirection_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("winddirection_175hPa"
             (:name . "175 hPa")
             (:type . checkbox))
            ("winddirection_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("winddirection_225hPa"
             (:name . "225 hPa")
             (:type . checkbox))
            ("winddirection_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("winddirection_275hPa"
             (:name . "275 hPa")
             (:type . checkbox))
            ("winddirection_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("winddirection_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("winddirection_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("winddirection_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("winddirection_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("winddirection_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("winddirection_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("winddirection_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("winddirection_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("winddirection_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("winddirection_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("winddirection_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("winddirection_875hPa"
             (:name . "875 hPa")
             (:type . checkbox))
            ("winddirection_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("winddirection_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("winddirection_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("winddirection_970hPa"
             (:name . "970 hPa")
             (:type . checkbox))
            ("winddirection_985hPa"
             (:name . "985 hPa")
             (:type . checkbox))
            ("winddirection_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("winddirection_1015hPa"
             (:name . "1015 hPa")
             (:type . checkbox))))
          ((:name . "Geopotential Height")
           (:fields
            ("geopotential_height_10hPa"
             (:name . "10 hPa")
             (:type . checkbox))
            ("geopotential_height_20hPa"
             (:name . "20 hPa")
             (:type . checkbox))
            ("geopotential_height_30hPa"
             (:name . "30 hPa")
             (:type . checkbox))
            ("geopotential_height_50hPa"
             (:name . "50 hPa")
             (:type . checkbox))
            ("geopotential_height_100hPa"
             (:name . "100 hPa")
             (:type . checkbox))
            ("geopotential_height_150hPa"
             (:name . "150 hPa")
             (:type . checkbox))
            ("geopotential_height_175hPa"
             (:name . "175 hPa")
             (:type . checkbox))
            ("geopotential_height_200hPa"
             (:name . "200 hPa")
             (:type . checkbox))
            ("geopotential_height_225hPa"
             (:name . "225 hPa")
             (:type . checkbox))
            ("geopotential_height_250hPa"
             (:name . "250 hPa")
             (:type . checkbox))
            ("geopotential_height_275hPa"
             (:name . "275 hPa")
             (:type . checkbox))
            ("geopotential_height_300hPa"
             (:name . "300 hPa")
             (:type . checkbox))
            ("geopotential_height_350hPa"
             (:name . "350 hPa")
             (:type . checkbox))
            ("geopotential_height_400hPa"
             (:name . "400 hPa")
             (:type . checkbox))
            ("geopotential_height_450hPa"
             (:name . "450 hPa")
             (:type . checkbox))
            ("geopotential_height_500hPa"
             (:name . "500 hPa")
             (:type . checkbox))
            ("geopotential_height_550hPa"
             (:name . "550 hPa")
             (:type . checkbox))
            ("geopotential_height_600hPa"
             (:name . "600 hPa")
             (:type . checkbox))
            ("geopotential_height_650hPa"
             (:name . "650 hPa")
             (:type . checkbox))
            ("geopotential_height_700hPa"
             (:name . "700 hPa")
             (:type . checkbox))
            ("geopotential_height_750hPa"
             (:name . "750 hPa")
             (:type . checkbox))
            ("geopotential_height_800hPa"
             (:name . "800 hPa")
             (:type . checkbox))
            ("geopotential_height_850hPa"
             (:name . "850 hPa")
             (:type . checkbox))
            ("geopotential_height_875hPa"
             (:name . "875 hPa")
             (:type . checkbox))
            ("geopotential_height_900hPa"
             (:name . "900 hPa")
             (:type . checkbox))
            ("geopotential_height_925hPa"
             (:name . "925 hPa")
             (:type . checkbox))
            ("geopotential_height_950hPa"
             (:name . "950 hPa")
             (:type . checkbox))
            ("geopotential_height_970hPa"
             (:name . "970 hPa")
             (:type . checkbox))
            ("geopotential_height_985hPa"
             (:name . "985 hPa")
             (:type . checkbox))
            ("geopotential_height_1000hPa"
             (:name . "1000 hPa")
             (:type . checkbox))
            ("geopotential_height_1015hPa"
             (:name . "1015 hPa")
             (:type . checkbox)))))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relativehumidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dewpoint_2m"
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
        ("weathercode"
         (:name . "Weathercode")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloudcover"
         (:name . "Cloudcover Total")
         (:type . checkbox))
        ("cloudcover_low"
         (:name . "Cloudcover Low")
         (:type . checkbox))
        ("cloudcover_mid"
         (:name . "Cloudcover Mid")
         (:type . checkbox))
        ("cloudcover_high"
         (:name . "Cloudcover High")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))
        ("vapor_pressure_deficit"
         (:name . "Vapor Pressure Deficit")
         (:type . checkbox))
        ("windspeed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("windspeed_40m"
         (:name . "Wind Speed (40 m)")
         (:type . checkbox))
        ("windspeed_80m"
         (:name . "Wind Speed (80 m)")
         (:type . checkbox))
        ("windspeed_120m"
         (:name . "Wind Speed (1200 m)")
         (:type . checkbox))
        ("winddirection_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("winddirection_40m"
         (:name . "Wind Direction (40 m)")
         (:type . checkbox))
        ("winddirection_80m"
         (:name . "Wind Direction (80 m)")
         (:type . checkbox))
        ("winddirection_120m"
         (:name . "Wind Direction (120 m)")
         (:type . checkbox))
        ("windgusts_10m"
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
        ("weathercode"
         (:name . "Weathercode")
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
        ("windspeed_10m_max"
         (:name . "Maximum Wind Speed (10 m)")
         (:type . checkbox))
        ("windgusts_10m_max"
         (:name . "Maximum Wind Gusts (10 m)")
         (:type . checkbox))
        ("winddirection_10m_dominant"
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
        ("windspeed_unit"
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
          ("unixtime" . "Unix timestamp")))
        ("timezone"
         (:type . timezone))
        ("past_days"
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("start_date"
         (:type . date))
        ("end_date"
         (:type . date))))))
    ("Historical Weather"
     (:name . "Historical Weather")
     (:url . "https://open-meteo.com/en/docs/historical-weather-api")
     (:description . "Weather information since 1940")
     (:key . "h")
     (:sections
      ((:name . "Select Coordinates or City")
       (:fields
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Additional Variables")
         (:fields
          ("is_day"
           (:name . "Is Day or Night")
           (:type . checkbox))))
        ((:name . "Solar Radiation Variables")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox)))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relativehumidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dewpoint_2m"
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
        ("showers"
         (:name . "Showers")
         (:type . checkbox))
        ("snowfall"
         (:name . "Snowfall")
         (:type . checkbox))
        ("weathercode"
         (:name . "Weathercode")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloudcover"
         (:name . "Cloudcover Total")
         (:type . checkbox))
        ("cloudcover_low"
         (:name . "Cloudcover Low")
         (:type . checkbox))
        ("cloudcover_mid"
         (:name . "Cloudcover Mid")
         (:type . checkbox))
        ("cloudcover_high"
         (:name . "Cloudcover High")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))
        ("vapor_pressure_deficit"
         (:name . "Vapor Pressure Deficit")
         (:type . checkbox))
        ("windspeed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("windspeed_100m"
         (:name . "Wind Speed (100 m)")
         (:type . checkbox))
        ("winddirection_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("winddirection_100m"
         (:name . "Wind Direction (100 m)")
         (:type . checkbox))
        ("windgusts_10m"
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
        ("weathercode"
         (:name . "Weathercode")
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
        ("windspeed_10m_max"
         (:name . "Maximum Wind Speed (10 m)")
         (:type . checkbox))
        ("windgusts_10m_max"
         (:name . "Maximum Wind Gusts (10 m)")
         (:type . checkbox))
        ("winddirection_10m_dominant"
         (:name . "Dominant Wind Direction (10 m)")
         (:type . checkbox))
        ("shortwave_radiation_sum"
         (:name . "Shortwave Radiation Sum")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))))
      ((:name . "Specify Time Interval")
       (:fields
        ("start_date"
         (:type . date))
        ("end_date"
         (:type . date))))
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
        ("windspeed_unit"
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
          ("unixtime" . "Unix timestamp")))
        ("timezone"
         (:type . timezone))))
      ((:param . "models")
       (:name . "Reanalysis models")
       (:fields
        ("best_match"
         (:name . "Best match")
         (:type . checkbox))
        ("era5"
         (:name . "ERA5")
         (:type . checkbox))
        ("era5_land"
         (:name . "ERA5-Land")
         (:type . checkbox))
        ("cerra"
         (:name . "CERRA")
         (:type . checkbox))))))
    ("Ensemble Models"
     (:name . "Ensemble Models")
     (:url . "https://open-meteo.com/en/docs/ensemble-api")
     (:description . "Weather information since 1940")
     (:key . "e")
     (:sections
      ((:name . "Select Coordinates or City")
       (:fields
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))))
      ((:param . "hourly")
       (:name . "Hourly Weather Variables")
       (:children
        ((:name . "Additional Variables")
         (:fields
          ("uv_index"
           (:name . "UV Index")
           (:type . checkbox))
          ("uv_index_clear_sky"
           (:name . "UV Index Clear Sky")
           (:type . checkbox))
          ("cape"
           (:name . "CAPE")
           (:type . checkbox))
          ("freezinglevel_height"
           (:name . "Freezinglevel Height")
           (:type . checkbox))))
        ((:name . "Solar Radiation Variables")
         (:fields
          ("shortwave_radiation"
           (:name . "Shortwave Solar Radiation")
           (:type . checkbox))
          ("direct_radiation"
           (:name . "Direct Solar Radiation")
           (:type . checkbox))
          ("diffuse_radiation"
           (:name . "Diffuse Solar Radiation")
           (:type . checkbox))
          ("direct_normal_irradiance"
           (:name . "Direct Normal Irradiance DNI")
           (:type . checkbox))
          ("shortwave_radiation_instant"
           (:name . "Shortwave Solar Radiation (Instant)")
           (:type . checkbox))
          ("direct_radiation_instant"
           (:name . "Direct Solar Radiation (Instant)")
           (:type . checkbox))
          ("diffuse_radiation_instant"
           (:name . "Diffuse Solar Radiation (Instant)")
           (:type . checkbox))
          ("direct_normal_irradiance_instant"
           (:name . "Direct Normal Irradiance DNI (Instant)")
           (:type . checkbox)))))
       (:fields
        ("temperature_2m"
         (:name . "Temperature (2 m)")
         (:type . checkbox))
        ("relativehumidity_2m"
         (:name . "Relative Humidity (2 m)")
         (:type . checkbox))
        ("dewpoint_2m"
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
        ("weathercode"
         (:name . "Weathercode")
         (:type . checkbox))
        ("pressure_msl"
         (:name . "Sealevel Pressure")
         (:type . checkbox))
        ("surface_pressure"
         (:name . "Surface Pressure")
         (:type . checkbox))
        ("cloudcover"
         (:name . "Cloudcover Total")
         (:type . checkbox))
        ("visibility"
         (:name . "Visibility")
         (:type . checkbox))
        ("et0_fao_evapotranspiration"
         (:name . "Reference Evapotranspiration (ET₀)")
         (:type . checkbox))
        ("vapor_pressure_deficit"
         (:name . "Vapor Pressure Deficit")
         (:type . checkbox))
        ("windspeed_10m"
         (:name . "Wind Speed (10 m)")
         (:type . checkbox))
        ("windspeed_80m"
         (:name . "Wind Speed (80 m)")
         (:type . checkbox))
        ("windspeed_120m"
         (:name . "Wind Speed (120 m)")
         (:type . checkbox))
        ("winddirection_10m"
         (:name . "Wind Direction (10 m)")
         (:type . checkbox))
        ("winddirection_80m"
         (:name . "Wind Direction (80 m)")
         (:type . checkbox))
        ("winddirection_120m"
         (:name . "Wind Direction (120 m)")
         (:type . checkbox))
        ("windgusts_10m"
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
        ("windspeed_unit"
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
          ("unixtime" . "Unix timestamp")))
        ("past_days"
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("forecast_days"
         (:type . number)
         (:min . 0)
         (:max . 16))
        ("start_date"
         (:type . date))
        ("end_date"
         (:type . date))
        ("timezone"
         (:type . timezone))))
      ((:param . "models")
       (:name . "Ensemble Models")
       (:fields
        ("icon_seamless"
         (:name . "DWD Icon EPS Seamless")
         (:type . checkbox))
        ("icon_global"
         (:name . "DWD Icon EPS Global")
         (:type . checkbox))
        ("icon_eu"
         (:name . "DWD Icon EPS EU")
         (:type . checkbox))
        ("icon_d2"
         (:name . "DWD Icon EPS D2")
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
         (:name . "ECMWF IFS Ensemble")
         (:type . checkbox))
        ("gem_global"
         (:name . "GEM Global Ensemble")
         (:type . checkbox))))))
    ("Climate Change"
     (:name . "Climate Change")
     (:url . "https://open-meteo.com/en/docs/climate-api")
     (:description . "Climate change projections")
     (:key . "c")
     (:sections
      ((:name . "Select Coordinates or City")
       (:fields
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))))
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
        ("windspeed_10m_mean"
         (:name . "Mean Wind Speed (10 m)")
         (:type . checkbox))
        ("windspeed_10m_max"
         (:name . "Max Wind Speed (10 m)")
         (:type . checkbox))
        ("cloudcover_mean"
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
        ("dewpoint_2m_mean"
         (:name . "Mean Dewpoint (2 m)")
         (:type . checkbox))
        ("dewpoint_2m_min"
         (:name . "Minimum Dewpoint (2 m)")
         (:type . checkbox))
        ("dewpoint_2m_max"
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
      ((:name . "Specify Time Interval")
       (:fields
        ("start_date"
         (:type . date))
        ("end_date"
         (:type . date))))
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
        ("windspeed_unit"
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
         (:name . "CMCC_CM2_VHR4")
         (:type . checkbox))
        ("FGOALS_f3_H"
         (:name . "FGOALS_f3_H")
         (:type . checkbox))
        ("HiRAM_SIT_HR"
         (:name . "HiRAM_SIT_HR")
         (:type . checkbox))
        ("MRI_AGCM3_2_S"
         (:name . "MRI_AGCM3_2_S")
         (:type . checkbox))
        ("EC_Earth3P_HR"
         (:name . "EC_Earth3P_HR")
         (:type . checkbox))
        ("MPI_ESM1_2_XR"
         (:name . "MPI_ESM1_2_XR")
         (:type . checkbox))
        ("NICAM16_8S"
         (:name . "NICAM16_8S")
         (:type . checkbox))))))
    ("Marine Forecast"
     (:name . "Marine Forecast")
     (:url . "https://open-meteo.com/en/docs/marine-weather-api")
     (:description . "Wave forecasts")
     (:key . "m")
     (:sections
      ((:name . "Select Coordinates or City")
       (:fields
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))))
      ((:name . "Hourly Marine Variables")
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
      ((:name . "Daily Marine Variables")
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
      ((:name . "Settings")
       (:fields
        ("timezone"
         (:type . timezone))
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
          ("unixtime" . "Unix timestamp")))
        ("past_days"
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("start_date"
         (:type . date))
        ("end_date"
         (:type . date))))))
    ("Air Quality"
     (:name . "Air Quality")
     (:url . "https://open-meteo.com/en/docs/air-quality-api")
     (:description . "Pollutants and pollen forcast")
     (:key . "a")
     (:sections
      ((:name . "Select Coordinates or City")
       (:fields
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))))
      ((:name . "Hourly Air Quality Variables")
       (:children
        ((:name . "European Air Quality Index")
         (:fields
          ("european_aqi"
           (:name . "European AQI")
           (:type . checkbox))
          ("european_aqi_pm2_5"
           (:name . "European AQI PM")
           (:type . checkbox))
          ("european_aqi_pm10"
           (:name . "European AQI PM")
           (:type . checkbox))
          ("european_aqi_no2"
           (:name . "European AQI NO")
           (:type . checkbox))
          ("european_aqi_o3"
           (:name . "European AQI O")
           (:type . checkbox))
          ("european_aqi_so2"
           (:name . "European AQI SO")
           (:type . checkbox))))
        ((:name . "United States Air Quality Index")
         (:fields
          ("us_aqi"
           (:name . "United States AQI")
           (:type . checkbox))
          ("us_aqi_pm2_5"
           (:name . "United States AQI PM")
           (:type . checkbox))
          ("us_aqi_pm10"
           (:name . "United States AQI PM")
           (:type . checkbox))
          ("us_aqi_no2"
           (:name . "United States AQI NO")
           (:type . checkbox))
          ("us_aqi_co"
           (:name . "United States AQI CO")
           (:type . checkbox))
          ("us_aqi_o3"
           (:name . "United States AQI O")
           (:type . checkbox))
          ("us_aqi_so2"
           (:name . "United States AQI SO")
           (:type . checkbox)))))
       (:fields
        ("pm10"
         (:name . "Particulate Matter PM")
         (:type . checkbox))
        ("pm2_5"
         (:name . "Particulate Matter PM")
         (:type . checkbox))
        ("carbon_monoxide"
         (:name . "Carbon Monoxide CO")
         (:type . checkbox))
        ("nitrogen_dioxide"
         (:name . "Nitrogen Dioxide NO")
         (:type . checkbox))
        ("sulphur_dioxide"
         (:name . "Sulphur Dioxide SO")
         (:type . checkbox))
        ("ozone"
         (:name . "Ozone O")
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
         (:name . "Ammonia NH")
         (:type . checkbox))
        ("alder_pollen"
         (:name . "Alder Pollen")
         (:type . checkbox))
        ("birch_pollen"
         (:name . "Birch Pollen")
         (:type . checkbox))
        ("grass_pollen"
         (:name . "Grass Pollen")
         (:type . checkbox))
        ("mugwort_pollen"
         (:name . "Mugwort Pollen")
         (:type . checkbox))
        ("olive_pollen"
         (:name . "Olive Pollen")
         (:type . checkbox))
        ("ragweed_pollen"
         (:name . "Ragweed Pollen")
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
        ("timezone"
         (:type . timezone))
        ("timeformat"
         (:name . "Timeformat")
         (:type . select)
         (:options
          ("iso8601" . "ISO 8601 (e.g. 2022-12-31)")
          ("unixtime" . "Unix timestamp")))
        ("past_days"
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("start_date"
         (:type . date))
        ("end_date"
         (:type . date))))))
    ("Flood"
     (:name . "Flood")
     (:url . "https://open-meteo.com/en/docs/flood-api")
     (:description . "River discharge forecast")
     (:key . "f")
     (:sections
      ((:name . "Select Coordinates or City")
       (:fields
        ("latitude"
         (:name . "Latitude")
         (:type . float))
        ("longitude"
         (:name . "Longitude")
         (:type . float))))
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
         (:name . "River Discharge 25 Percentile")
         (:type . checkbox))
        ("river_discharge_p75"
         (:name . "River Discharge 75 Percentile")
         (:type . checkbox))
        ("ensemble"
         (:name . "All 50 Ensemble Members")
         (:type . checkbox))))
      ((:name . "Settings")
       (:fields
        ("start_date"
         (:type . date))
        ("end_date"
         (:type . date))
        ("past_days"
         (:type . number)
         (:min . 0)
         (:max . 92))
        ("forecast_days"
         (:type . number)
         (:min . 0)
         (:max . 16))
        ("timeformat"
         (:name . "Timeformat")
         (:type . select)
         (:options
          ("iso8601" . "ISO 8601 (e.g. 2022-12-31)")
          ("unixtime" . "Unix timestamp")))))
      ((:param . "models")
       (:name . "Flood Models")
       (:fields
        ("seamless_v3"
         (:name . "GloFAS v3 Seamless")
         (:type . checkbox))
        ("forecast_v3"
         (:name . "GloFAS v3 Forecast")
         (:type . checkbox))
        ("consolidated_v3"
         (:name . "GloFAS v3 Consolidated")
         (:type . checkbox))
        ("seamless_v4"
         (:name . "GloFAS v4 Seamless")
         (:type . checkbox))
        ("forecast_v4"
         (:name . "GloFAS v4 Forecast")
         (:type . checkbox))
        ("consolidated_v4"
         (:name . "GloFAS v4 Consolidated")
         (:type . checkbox)))))))
  "open-meteo API docs data.\nCheck `biome-api-parse--page' for the format.")


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
