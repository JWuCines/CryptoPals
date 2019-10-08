package eu.jiangwu.cryptopals
package settwo

object S2_C9_PCKS7Padding {
    def run {
        val block: Array[Byte] = "YELLOW SUBMARINE".getBytes
        val blockSize: Int = 20
        Console.println("Block " + block.toCharString + " size: " + block.length + " - Required size: " + blockSize)
        val paddedBlock: Array[Byte] = padding(block, blockSize)
        assert(paddedBlock.length == blockSize)
        Console.println("Padded block (hex) " + paddedBlock.toHexString + " size: " + paddedBlock.length)
        val depaddedBlock: Array[Byte] = remove_padding(paddedBlock)
        assert(depaddedBlock.length == block.length)
        Console.println("Depadded block (hex) " + depaddedBlock.toHexString + " size: " + depaddedBlock.length)
        Console.println("Depadded block (ascii) " + depaddedBlock.toCharString + " size: " + depaddedBlock.length)
    }

    def padding(bytes: Array[Byte], blockSize: Integer, paddingByte: Byte = 4): Array[Byte] =
        bytes ++ paddingByte.multiple(blockSize - bytes.length)

    def remove_padding(bytes: Array[Byte], paddingByte: Byte = 4): Array[Byte] =
        bytes.reverse.dropWhile(_ == paddingByte).reverse
}
