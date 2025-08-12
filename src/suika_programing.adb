with System; use System;

with Interfaces.C; use Interfaces.C;
with Ada.Unchecked_Conversion;

with Box2D; use Box2D;
with Box2D_Raylib_Debug;

with Raylib; use Raylib;
with Raylib.GUI;
with Resources;
with Suika_Programing_Config;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Numerics.Discrete_Random;

procedure Suika_Programing is
   pragma Suppress_All;
   pragma Style_Checks ("M120");

   package Game_Resources
   is new Resources (Suika_Programing_Config.Crate_Name);

   Width  : constant := 800;
   Height : constant := 600;

   type Object_Kind is (Cherry, Grape, Strawberry, Orange, Apple, Pear,
                        Dekopon, Peach, Pineapple, Melon, Watermelon)
     with Size => Standard'Address_Size;

   for Object_Kind use
     (Cherry => 1,
      Grape => 2,
      Strawberry => 3,
      Orange => 4,
      Apple => 5,
      Pear => 6,
      Dekopon => 7,
      Peach => 8,
      Pineapple => 9,
      Melon => 10,
      Watermelon => 11);

   function Kind_To_Addr
   is new Ada.Unchecked_Conversion (Object_Kind, System.Address);
   function Addr_To_Kind
   is new Ada.Unchecked_Conversion (System.Address, Object_Kind);

   subtype Drop_Object_Kind is Object_Kind range Object_Kind'First .. Pear;
   package Random_Object is new Ada.Numerics.Discrete_Random (Drop_Object_Kind);
   Obj_Gen : Random_Object.Generator;

   Base_Radius       : constant := 6.0;
   Cherry_Radius     : constant := Base_Radius       * 1.4;
   Grape_Radius      : constant := Cherry_Radius     * 1.4;
   Strawberry_Radius : constant := Grape_Radius      * 1.4;
   Orange_Radius     : constant := Strawberry_Radius * 1.4;
   Apple_Radius      : constant := Orange_Radius     * 1.4;
   Pear_Radius       : constant := Apple_Radius      * 1.4;
   Dekopon_Radius    : constant := Pear_Radius       * 1.3;
   Peach_Radius      : constant := Dekopon_Radius    * 1.3;
   Pineapple_Radius  : constant := Peach_Radius      * 1.3;
   Melon_Radius      : constant := Pineapple_Radius  * 1.2;
   Watermelon_Radius : constant := Melon_Radius      * 1.2;

   Radius : constant array (Object_Kind) of C_float :=
     (Cherry      => Cherry_Radius,
      Grape       => Grape_Radius,
      Strawberry  => Strawberry_Radius,
      Orange      => Orange_Radius,
      Apple       => Apple_Radius,
      Pear        => Pear_Radius,
      Dekopon     => Dekopon_Radius,
      Peach       => Peach_Radius,
      Pineapple   => Pineapple_Radius,
      Melon       => Melon_Radius,
      Watermelon  => Watermelon_Radius);

   Points : constant array (Object_Kind) of Natural :=
     (Cherry      => 1,
      Grape       => 3,
      Strawberry  => 6,
      Orange      => 10,
      Apple       => 15,
      Pear        => 21,
      Dekopon     => 28,
      Peach       => 36,
      Pineapple   => 45,
      Melon       => 55,
      Watermelon  => 66);

   --  function Mass (Kind : Object_Kind) return C_float
   --  is (cpAreaForCircle (Radius (Kind), 0.0));
   --
   --  function Moment (Kind : Object_Kind) return C_float
   --  is (cpMomentForCircle (Mass (Kind), Radius (Kind), 0.0, (0.0, 0.0)));

   function Elasticity (Kind : Object_Kind) return C_float
   is (case Kind is
          when others => 0.5);

   function Friction (Kind : Object_Kind) return C_float
   is (case Kind is
          when others => 0.5);

   function Image_File (Kind : Object_Kind) return String
   is (case Kind is
          when Watermelon => "ada_logo.png",
          when Cherry => "CPlusPlus.png",
          when Grape => "OCaml.png",
          when Melon => "Fortran.png",
          when Orange => "Lua.png",
          when Apple => "CSharp.png",
          when Peach => "PHP.png",
          when Dekopon => "Python.png",
          when Pineapple => "Ruby.png",
          when Strawberry => "Rust.png",
          when Pear => "Swift.png");

   Images : array (Object_Kind) of Raylib.Image;
   Textures : array (Object_Kind) of Raylib.Texture;

   type Merge_Data is record
      Kind : Object_Kind;
      A, B : BodyId;
   end record;

   package Merge_Lists is new Ada.Containers.Doubly_Linked_Lists (Merge_Data);

   Bodies_To_Merge : Merge_Lists.List;

   World_Id : WorldId := (0, 0);

   type Circle_Data is record
      Id : BodyId;
      Kind : Object_Kind;
   end record;

   package Circles_Lists
   is new Ada.Containers.Doubly_Linked_Lists (Circle_Data);
   Circles : Circles_Lists.List;

   procedure Add_Object (X, Y : C_float; Kind : Object_Kind) is

      Body_Def : aliased BodyDef := DefaultBodyDef;
      Body_Id : BodyId;

      Shape_Def : aliased ShapeDef := DefaultShapeDef;
      C : aliased Circle;

   begin

      Body_Def.position := (X, Y);
      Body_Def.type_K := DynamicBody;
      Body_Id := CreateBody (World_Id, Body_Def'Access);

      Shape_Def.material.restitution := Elasticity (Kind);
      Shape_Def.material.friction := Friction (Kind);
      Shape_Def.userData := Kind_To_Addr (Kind);
      Shape_Def.enableContactEvents := True;
      C.center := (0.0, 0.0);
      C.radius := Radius (Kind);
      CreateCircleShape (Body_Id, Shape_Def'Access, C'Access);

      Circles.Append ((Body_Id, Kind));
   end Add_Object;
   type Segment_Data is record
      A, B : Vec2;
      Radius : C_float;
      Bod : BodyId;
   end record;

   package Segment_Lists
   is new Ada.Containers.Doubly_Linked_Lists (Segment_Data);

   Segments : Segment_Lists.List;

   procedure Add_Segment (A, B : Vec2; Radius : C_float) is
      Body_Def : aliased BodyDef := DefaultBodyDef;
      Body_Id : BodyId;

      Shape_Def : aliased ShapeDef := DefaultShapeDef;
      Seg : aliased Segment := (A, B);

   begin

      Body_Def.type_K := StaticBody;
      Body_Def.position := (0.0, 0.0);
      Body_Id := CreateBody (World_Id, Body_Def'Access);

      Shape_Def.material.restitution := 0.5;
      Shape_Def.material.friction := 100.0;
      CreateSegmentShape (Body_Id, Shape_Def'Access, Seg'Access);

      Segments.Append ((A, B, Radius, Body_Id));
   end Add_Segment;

   -----------------
   -- Draw_Object --
   -----------------

   procedure Draw_Object (Kind : Object_Kind; Pos : Vec2; Angle : C_float) is
      Tex : Texture renames Textures (Kind);
      Rad : constant C_float := Radius (Kind);
   begin
      DrawTexturePro (Tex,
                      source => (0.0, 0.0,
                                 C_float (Tex.width), C_float (Tex.height)),
                      dest =>  (Pos.x,
                                C_float (Height) - Pos.y,
                                Rad * 2.0,
                                Rad * 2.0),
                      origin => (Rad, Rad),
                      rotation => -Angle * 45.0,
                      tint =>  Raylib.WHITE);
   end Draw_Object;

   Gameover : Boolean := False;
   Score : Natural := 0;

   Next_To_Drop  : Object_Kind := Object_Kind'First;
   Drop_Timeout  : Ada.Real_Time.Time := Ada.Real_Time.Clock;
   Drop_Height   : constant C_float := C_float (Height) - 40.0;
   Drop_Interval : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Milliseconds (300);

   procedure Setup_Game is
      World_Def : aliased WorldDef := DefaultWorldDef;
   begin
      Score := 0;
      Next_To_Drop := Object_Kind'First;
      Drop_Timeout := Ada.Real_Time.Clock;
      Gameover := False;
      Random_Object.Reset (Obj_Gen);

      Segments.Clear;
      Circles.Clear;

      if World_Id /= (0, 0) then
         DestroyWorld (World_Id);
      end if;

      World_Def.gravity := (0.0, -900.0);
      World_Id := CreateWorld (World_Def'Access);

      --  Set borders
      Add_Segment ((C_float (Width) * 0.25, 5.0),
                   (C_float (Width) * 0.75, 5.0),
                   5.0);
      Add_Segment ((C_float (Width) * 0.25, C_float (Height) * 0.0),
                   (C_float (Width) * 0.25, C_float (Height) * 0.8),
                   5.0);
      Add_Segment ((C_float (Width) * 0.75, C_float (Height) * 0.8),
                   (C_float (Width) * 0.75, C_float (Height) * 0.0),
                   5.0);
   end Setup_Game;

   Period : constant Ada.Real_Time.Time_Span :=
     Ada.Real_Time.Milliseconds (1000 / 60);

   Next_Release : Ada.Real_Time.Time;

   ---------------
   -- To_Raylib --
   ---------------

   function To_Raylib (Pos : Vec2) return Raylib.Vector2 is
   begin
      return (Pos.x, C_float (Height) - Pos.y);
   end To_Raylib;

   Debug_Enabled : Boolean := False;
   Debug_Draw : aliased DebugDraw := DefaultDebugDraw;
   package Dbg_Draw is new Box2D_Raylib_Debug (To_Raylib);
begin
   Debug_Draw.drawShapes := True;
   Debug_Draw.drawBodyNames := True;
   Debug_Draw.drawBounds := True;
   Debug_Draw.drawShapes := True;
   Debug_Draw.drawJoints := True;
   Debug_Draw.drawJointExtras := True;
   Debug_Draw.drawBounds := True;
   Debug_Draw.drawMass := False;
   Debug_Draw.drawBodyNames := True;
   Debug_Draw.drawContacts := True;
   Debug_Draw.drawGraphColors := True;
   Debug_Draw.drawContactNormals := True;
   Debug_Draw.drawContactImpulses := True;
   Debug_Draw.drawContactFeatures := True;
   Debug_Draw.drawFrictionImpulses := True;
   Debug_Draw.drawIslands := True;

   Dbg_Draw.Setup_Draw_Debug (Debug_Draw);

   Raylib.InitWindow (Width, Height, "Watermelon Chipmunk Example");

   for K in Object_Kind loop
      Images (K) := Raylib.LoadImage
        (Game_Resources.Resource_Path & Image_File (K));
      Textures (K) := Raylib.LoadTextureFromImage (Images (K));
   end loop;

   Setup_Game;

   Next_Release := Ada.Real_Time.Clock + Period;
   while not WindowShouldClose loop

      delay until Next_Release;
      Next_Release := Next_Release + Period;

      if not Gameover
        and then
          Drop_Timeout < Clock
        and then
          Boolean (IsMouseButtonPressed (MOUSE_BUTTON_LEFT))
      then
         Drop_Timeout := Clock + Drop_Interval;

         Add_Object (GetMousePosition.x,
                     Drop_Height,
                     Next_To_Drop);

         Next_To_Drop := Random_Object.Random (Obj_Gen);
      end if;

      if IsKeyPressed (KEY_F1) then
         Debug_Enabled := not Debug_Enabled;
      end if;

      if not Gameover then
         Bodies_To_Merge.Clear;
         Step (World_Id, 1.0 / 60.0, 100);

         declare
            Contacts : constant ContactEvents :=
              GetContactEvents (World_Id);
         begin
            if Contacts.beginCount > 0 then
               for Idx in unsigned range  0 .. unsigned (Contacts.beginCount - 1)
               loop
                  declare
                     Shape_A : constant ShapeId :=
                       Contacts.beginEvents (Idx).shapeIdA;
                     Shape_B : constant ShapeId :=
                       Contacts.beginEvents (Idx).shapeIdB;

                     Data_A : constant System.Address :=
                       GetUserData (Shape_A);
                     Data_B : constant System.Address :=
                       GetUserData (Shape_B);
                  begin
                     if Data_A /= System.Null_Address
                       and then
                         Data_A = Data_B
                     then
                        Bodies_To_Merge.Append
                          ((Kind => Addr_To_Kind (Data_A),
                            A => GetBody (Shape_A),
                            B => GetBody (Shape_B)));
                     end if;
                  end;
               end loop;
            end if;
         end;

         for Elt of Bodies_To_Merge loop
            if IsValid (Elt.A) and then IsValid (Elt.B) then
               declare
                  A_Pos : constant Vec2 := GetPosition (Elt.A);
                  B_Pos : constant Vec2 := GetPosition (Elt.B);
               begin
                  DestroyBody (Elt.A);
                  DestroyBody (Elt.B);

                  Score := Score + Points (Elt.Kind);

                  if Elt.Kind /= Object_Kind'Last then
                     --  Merging the last object doesn't produce a new object
                     Add_Object ((A_Pos.x + B_Pos.x) / 2.0,
                                 (A_Pos.y + B_Pos.y) / 2.0,
                                 Object_Kind'Succ (Elt.Kind));
                  end if;
               end;
            end if;
         end loop;
      end if;

      BeginDrawing;
      ClearBackground (BLACK);
      --  Debug.Draw_Debug (Space);

      if not Gameover and then Drop_Timeout < Ada.Real_Time.Clock then
         Draw_Object (Next_To_Drop,
                      (GetMousePosition.x, Drop_Height),
                      0.0);
      end if;

      DrawRectangle
        (int (C_float (Width) * 0.75) - 5,
         int (C_float (Height) * 0.2),
         10,
         int (C_float (Height) * 0.8),
         Raylib.WHITE);

      DrawRectangle
        (int (C_float (Width) * 0.25) - 5,
         int (C_float (Height) * 0.2),
         10,
         int (C_float (Height) * 0.8),
         Raylib.WHITE);

      DrawRectangle
        (int (C_float (Width) * 0.25),
         int (C_float (Height) * 1.0) - 10,
         int (C_float (Width) * 0.5),
         10,
         Raylib.WHITE);

      for C of Circles loop
         if IsValid (C.Id) then
            declare
               P : constant Vec2 := GetPosition (C.Id);
               Angle : constant C_float := GetAngle (GetRotation (C.Id));
            begin

               if P.y < -20.0 then
                  Gameover := True;
               end if;

               Draw_Object (C.Kind, (P.x, P.y), Angle);
            end;
         end if;
      end loop;

      DrawText (text => "Score:" & Score'Img,
                posX => 0,
                posY => 40,
                fontSize => 20,
                color_p => Raylib.RED);

      if Gameover then
         declare
            Str : constant String := "GameOver!";
            Size : constant := 100;
            W : constant int := MeasureText (Str, Size);
         begin
            DrawText (text => Str,
                      posX => Width / 2 - W / 2,
                      posY => Height / 2,
                      fontSize => 100,
                      color_p => Raylib.RED);

            if Raylib.GUI.GuiButton ((C_float (Width) * 0.4,
                                     C_float (Height) * 0.7,
                                     C_float (Width) * 0.2,
                                     C_float (Height) * 0.1),
                                     "Restart") /= 0
              or else
                Boolean (Raylib.IsKeyPressed (Raylib.KEY_ENTER))
            then
               Setup_Game;
            end if;
         end;
      end if;

      if Debug_Enabled then
         Draw (World_Id, Debug_Draw'Access);
         Raylib.DrawFPS (0, 0);
      end if;
      EndDrawing;
   end loop;

   CloseWindow;
end Suika_Programing;
