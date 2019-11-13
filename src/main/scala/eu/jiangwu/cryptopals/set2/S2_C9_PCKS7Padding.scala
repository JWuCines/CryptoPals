package eu.jiangwu.cryptopals
package set2

object S2_C9_PCKS7Padding {
    def run {
        val block: Array[Byte] = "YELLOW SUBMARINE".getBytes
        val blockSize: Int = 20
        Console.println("Block " + block.toCharString + " size: " + block.length + " - Required size: " + blockSize)
        val paddedBlock: Array[Byte] = padding(block, blockSize)
        assert(paddedBlock.length == blockSize)
        Console.println("Padded block (hex) " + paddedBlock.toHexString + " size: " + paddedBlock.length)
        val depaddedBlock: Array[Byte] = removePadding(paddedBlock)
        assert(depaddedBlock.length == block.length)
        Console.println("Depadded block (hex) " + depaddedBlock.toHexString + " size: " + depaddedBlock.length)
        Console.println("Depadded block (ascii) " + depaddedBlock.toCharString + " size: " + depaddedBlock.length)
    }

    def padding(bytes: Array[Byte], blockSize: Integer = AES_LENGTH): Array[Byte] =
        bytes.grouped(blockSize).flatMap(data => data ++ (blockSize - data.length).toByte.multiple(blockSize - data.length)).toArray

    def removePadding(bytes: Array[Byte], blockSize: Int = AES_LENGTH): Array[Byte] = {
        if (bytes.endsWith(bytes.last.multiple(bytes.last.toInt))) bytes.dropRight(bytes.last.toInt) else bytes
    }
}
