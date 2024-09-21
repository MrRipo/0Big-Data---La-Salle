package labrecu;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;

import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.io.Writable;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Partitioner;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

public class AnalistaPelis extends Configured implements Tool {

	public class MovieWritable implements Writable {

		@Override
		public void readFields(DataInput input) throws IOException {
		}

		@Override
		public void write(DataOutput output) throws IOException {
		}

	}

	public class GenrePartitioner extends Partitioner<Text, MovieWritable>{

		@Override
		public int getPartition(Text text, MovieWritable value, int numpartitions) {
			return 0;
		}
	}

	public class MovieReducer extends Reducer<Text, MovieWritable, Text, Text>{
	}

	public class MovieMapper extends Mapper<LongWritable, Text, Text, MovieWritable>{
	}

	@Override
	public int run(String[] args) throws Exception {
		Job job = Job.getInstance(getConf(), "Movie Analysis");
		job.setJarByClass(AnalistaPelis.class);
		job.setMapperClass(MovieMapper.class);
		job.setReducerClass(MovieReducer.class);
		job.setPartitionerClass(GenrePartitioner.class);
		job.setNumReduceTasks(3);
		job.setOutputKeyClass(Text.class);
		job.setOutputValueClass(MovieWritable.class);
		FileInputFormat.addInputPath(job, new Path(args[0]));
		FileOutputFormat.setOutputPath(job, new Path(args[1]));
		job.getConfiguration().setFloat("revenue.threshold", Float.parseFloat(args[2]));
		return job.waitForCompletion(true) ? 0 : 1;
	}

	/**
	 * Punto de entrada de ejecución del job.
	 * 
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception {
		if (args.length != 3) {
			System.err.println("Us: AnalistaPelis <entrada> <sortida> <ingresos>");
			System.exit(-1);
		}
		int codiSortida = ToolRunner.run(new AnalistaPelis(), args);
		System.exit(codiSortida);
	}
}
