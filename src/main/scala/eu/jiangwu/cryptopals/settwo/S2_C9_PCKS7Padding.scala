package eu.jiangwu.cryptopals
package settwo

object S2_C9_PCKS7Padding {
    def run {
        val block: String = "YELLOW SUBMARINE"
        val block_size: Int = 20
        Console.println("Block " + block + " size: " + block.length + " - Required size: " + block_size)
        val padded_block: Array[Byte] = padding(block.getBytes, block_size)
        Console.println("Padded block (hex) " + padded_block.toHexString + " size: " + padded_block.length)
    }

    def padding(bytes: Array[Byte], block_size: Integer, paddingByte: Byte = 4): Array[Byte] =
        bytes ++ (paddingByte.toChar.toString * (block_size - bytes.length)).getBytes
}
