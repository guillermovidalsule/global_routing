with Ada.Containers.Vectors;

package Nets is

   type Coordinate_Array is array (Integer range <>) of Integer;

   type Net_Record (Degree : Positive) is
     record
      X, Y : Coordinate_Array (1 .. Degree);
     end record;

   type Net_Ptr is access Net_Record;

   package Net_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Net_Ptr);

end Nets;
