import struct


class Las_Header:
    "Class to read LAS header file information"

    def __init__(self, file_name):
        self.file_name = file_name
        self.version = self.find_version()
        self.extent = self.find_extent()
        self.xmax = self.extent[0]
        self.xmin = self.extent[1]
        self.ymax = self.extent[2]
        self.ymin = self.extent[3]
        self.zmax = self.extent[4]
        self.zmin = self.extent[5]

    def find_version(self):
        with open(self.file_name, "rb") as file:
            payload = file.read(26)
        version_major = struct.unpack("b", payload[24:25])[0]
        version_minor = struct.unpack("b", payload[25:26])[0]
        version = str(version_major) + "." + str(version_minor)
        return version

    def find_offset_to_points(self):
        with open(self.file_name, "rb") as file:
            payload = file.read(100)
        offset_to_point_data = struct.unpack("I", payload[96:100])[0]

        return offset_to_point_data

    def first_pass(self):
        with open(self.file_name, "rb") as file:
            payload = file.read(100)
        version_major = struct.unpack("b", payload[24:25])[0]
        version_minor = struct.unpack("b", payload[25:26])[0]
        version = str(version_major) + "." + str(version_minor)
        offset_to_point_data = struct.unpack("I", payload[96:100])[0]
        return version, offset_to_point_data

    def read_header(self):
        # Read entire header
        version, offset_to_point_data = self.first_pass()

        with open(self.file_name, "rb") as file:
            payload = file.read(offset_to_point_data + 1)

        return payload

    def find_extent(self):
        version, offset_to_point_data = self.first_pass()

        with open(self.file_name, "rb") as file:
            payload = file.read(offset_to_point_data)

        if version == "1.2":
            max_x = struct.unpack("d", payload[179:187])[0]
            min_x = struct.unpack("d", payload[187:195])[0]
            max_y = struct.unpack("d", payload[195:203])[0]
            min_y = struct.unpack("d", payload[203:211])[0]
            max_z = struct.unpack("d", payload[211:219])[0]
            min_z = struct.unpack("d", payload[219:227])[0]

        if version == "1.3":
            max_x = struct.unpack("d", payload[187:195])[0]
            min_x = struct.unpack("d", payload[195:203])[0]
            max_y = struct.unpack("d", payload[203:211])[0]
            min_y = struct.unpack("d", payload[211:219])[0]
            max_z = struct.unpack("d", payload[219:227])[0]
            max_z = struct.unpack("d", payload[227:235])[0]

        if version == "1.4":
            max_x = struct.unpack("d", payload[179:187])[0]
            min_x = struct.unpack("d", payload[187:195])[0]
            max_y = struct.unpack("d", payload[195:203])[0]
            min_y = struct.unpack("d", payload[203:211])[0]
            max_z = struct.unpack("d", payload[211:219])[0]
            min_z = struct.unpack("d", payload[219:227])[0]

        return [max_x, min_x, max_y, min_y, max_z, min_z]
