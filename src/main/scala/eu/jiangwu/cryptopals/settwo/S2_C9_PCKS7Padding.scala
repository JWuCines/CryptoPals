package eu.jiangwu.cryptopals
package settwo

object S2_C9_PCKS7Padding {
    def run {
        val block: Array[Byte] = "YELLOW SUBMARINE".getBytes
        val block_size: Int = 20
        Console.println("Block " + block.toCharString + " size: " + block.length + " - Required size: " + block_size)
        val padded_block: Array[Byte] = padding(block, block_size)
        assert(padded_block.length == block_size)
        Console.println("Padded block (hex) " + padded_block.toHexString + " size: " + padded_block.length)
        val depadded_block: Array[Byte] = remove_padding(padded_block)
        assert(depadded_block.length == block.length)
        Console.println("Depadded block (hex) " + depadded_block.toHexString + " size: " + depadded_block.length)
        Console.println("Depadded block (ascii) " + depadded_block.toCharString + " size: " + depadded_block.length)
    }

    def padding(bytes: Array[Byte], block_size: Integer, paddingByte: Byte = 4): Array[Byte] =
        bytes ++ paddingByte.multiple(block_size - bytes.length)

    def remove_padding(bytes: Array[Byte], paddingByte: Byte = 4): Array[Byte] =
        bytes.reverse.dropWhile(_ == paddingByte).reverse
}
