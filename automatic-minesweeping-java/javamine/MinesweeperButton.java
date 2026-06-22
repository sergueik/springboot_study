package cs305.minesweeper.javamine;

import java.awt.Color;

import javax.swing.JButton;

import cs305.minesweeper.MinesweeperTile;

@SuppressWarnings("serial")//suppresses warning
public class MinesweeperButton extends JButton{
	
	private MinesweeperTile tile;
	
	public MinesweeperButton(MinesweeperTile t, String name) {
		super(name);
		this.tile = t;
	}
	
	public void updateTile(MinesweeperTile t){
		this.tile = t;
		String tileValue = String.valueOf(tile.getValue());

		if(tile.getValue() == MinesweeperTile.BOMB_VALUE){
			setForeground(Color.BLACK);
			setText("B");//set to what ever I want to represent a bomb
		}else if (tile.getValue() == MinesweeperTile.EMPTY_VALUE){
			setText("  ");
		}else if(tileValue.equalsIgnoreCase("1")){
			setForeground(Color.BLUE);
		}else if(tileValue.equalsIgnoreCase("2")){
			setForeground(Color.GREEN);
		}else if(tileValue.equalsIgnoreCase("3")){
			setForeground(Color.RED);
		}else if(tileValue.equalsIgnoreCase("4")){
			setForeground(Color.MAGENTA);
		}else if(tileValue.equalsIgnoreCase("5")){
			setForeground(Color.PINK);
		}else if(tileValue.equalsIgnoreCase("6")){
			setForeground(Color.ORANGE);
		}else if(tileValue.equalsIgnoreCase("7")){
			setForeground(Color.CYAN);
		}else if(tileValue.equalsIgnoreCase("8")){
			setForeground(Color.YELLOW);
		}

		if(tile.getValue() != MinesweeperTile.BOMB_VALUE && tile.getValue() != MinesweeperTile.EMPTY_VALUE)
			setText(tileValue);	
	}
	
	
	public MinesweeperTile getTile(){
		return this.tile;
	}

	
}
